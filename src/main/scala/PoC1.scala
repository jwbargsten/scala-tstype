// build.sbt
//
// scalaVersion := "3.7.0"
// No external dependencies.

// ============================================================
// 1. TypeScript AST — TSExpr
// ============================================================

object PoC1 {
  enum TSExpr:
    case TSPrimitive(name: String)
    case TSArray(element: TSExpr)
    case TSTuple(elements: List[TSExpr])
    case TSUnion(members: List[TSExpr])
    case TSInterface(name: String, fields: List[TSField])
    case TSRecord(key: TSExpr, value: TSExpr)
    case TSTypeAlias(name: String, underlying: TSExpr)
    case TSTypeReference(name: String)
    case TSLiteral(value: String)
    case TSNullable(inner: TSExpr)

  case class TSField(name: String, tpe: TSExpr)

  // ============================================================
  // 2. TSExpr serialization
  // ============================================================

  object TSEmitter:

    def emit(tpe: TSExpr): String = tpe match
      case TSExpr.TSPrimitive(n)     => n
      case TSExpr.TSArray(e)         => s"${emit(e)}[]"
      case TSExpr.TSTuple(es)        => es.map(emit).mkString("[", ", ", "]")
      case TSExpr.TSUnion(ms)        => ms.map(emit).mkString(" | ")
      case TSExpr.TSRecord(k, v)     => s"Record<${emit(k)}, ${emit(v)}>"
      case TSExpr.TSTypeAlias(n, u)  => s"type $n = ${emit(u)};"
      case TSExpr.TSTypeReference(n) => n
      case TSExpr.TSLiteral(v)       => v
      case TSExpr.TSNullable(i)      => s"${emit(i)} | null"
      case iface: TSExpr.TSInterface => emitInterface(iface)

    def emitInterface(i: TSExpr.TSInterface): String =
      val fields = i.fields.map(f => s"  ${f.name}: ${emit(f.tpe)};").mkString("\n")
      s"interface ${i.name} {\n$fields\n}"

    def collectDefinitions(tpe: TSExpr): List[TSExpr] =
      val seen = scala.collection.mutable.LinkedHashSet.empty[String]

      def go(t: TSExpr): List[TSExpr] = t match
        case i: TSExpr.TSInterface if seen.add(i.name) =>
          i.fields.flatMap(f => go(f.tpe)) :+ i
        case TSExpr.TSTypeAlias(n, u) if seen.add(n) =>
          go(u) :+ t
        case TSExpr.TSUnion(ms)    => ms.flatMap(go)
        case TSExpr.TSArray(e)     => go(e)
        case TSExpr.TSTuple(es)    => es.flatMap(go)
        case TSExpr.TSNullable(i)  => go(i)
        case TSExpr.TSRecord(k, v) => go(k) ++ go(v)
        case _                     => Nil

      go(tpe)

    def emitAll(tpe: TSExpr): String =
      collectDefinitions(tpe).map(emit).mkString("\n\n")

  // ============================================================
  // 3. TsType typeclass — the thing users write `derives`
  // ============================================================

  import java.time.*
  import java.util.UUID

  trait TsType[A]:
    def tsType: TSExpr

  object TsType:

    inline def apply[A](using t: TsType[A]): TsType[A] = t

    def instance[A](tpe: TSExpr): TsType[A] =
      new TsType[A] {
        val tsType = tpe
      }

    // ---- Primitives ----
    given TsType[String] = instance(TSExpr.TSPrimitive("string"))

    given TsType[Char] = instance(TSExpr.TSPrimitive("string"))

    given TsType[Boolean] = instance(TSExpr.TSPrimitive("boolean"))

    given TsType[Int] = instance(TSExpr.TSPrimitive("number"))

    given TsType[Long] = instance(TSExpr.TSPrimitive("number"))

    given TsType[Double] = instance(TSExpr.TSPrimitive("number"))

    given TsType[Float] = instance(TSExpr.TSPrimitive("number"))

    given TsType[Short] = instance(TSExpr.TSPrimitive("number"))

    given TsType[Byte] = instance(TSExpr.TSPrimitive("number"))

    given TsType[BigDecimal] = instance(TSExpr.TSPrimitive("number"))

    given TsType[BigInt] = instance(TSExpr.TSPrimitive("number"))

    // ---- Java/Scala standard types ----
    given TsType[UUID] = instance(TSExpr.TSPrimitive("string"))

    given TsType[LocalDate] = instance(TSExpr.TSPrimitive("string"))

    given TsType[LocalDateTime] = instance(TSExpr.TSPrimitive("string"))

    given TsType[Instant] = instance(TSExpr.TSPrimitive("string"))

    given TsType[OffsetDateTime] = instance(TSExpr.TSPrimitive("string"))

    given TsType[ZonedDateTime] = instance(TSExpr.TSPrimitive("string"))

    given TsType[Duration] = instance(TSExpr.TSPrimitive("string"))

    // ---- Option → T | null ----
    given optionTs: [A: TsType] => TsType[Option[A]] =
      instance(TSExpr.TSNullable(TsType[A].tsType))

    // ---- Any Iterable → T[] ----
    given iterableTs: [E, F[_]] => (e: TsType[E]) => (F[E] <:< Iterable[E]) => TsType[F[E]] =
      instance(TSExpr.TSArray(e.tsType))

    // ---- Map → Record<K, V> ----
    given mapTs: [K: TsType, V: TsType] => TsType[Map[K, V]] =
      instance(TSExpr.TSRecord(TsType[K].tsType, TsType[V].tsType))

    // ---- Either → A | B ----
    given eitherTs: [L: TsType, R: TsType] => TsType[Either[L, R]] =
      instance(TSExpr.TSUnion(List(TsType[L].tsType, TsType[R].tsType)))

    // ---- Tuples ----
    given tuple2: [A: TsType, B: TsType] => TsType[(A, B)] =
      instance(TSExpr.TSTuple(List(TsType[A].tsType, TsType[B].tsType)))

    given tuple3: [A: TsType, B: TsType, C: TsType] => TsType[(A, B, C)] =
      instance(TSExpr.TSTuple(List(TsType[A].tsType, TsType[B].tsType, TsType[C].tsType)))

    // ---- Convenience factories (à la scala-tsi) ----

    def sameAs[A, B](using b: TsType[B]): TsType[A] = instance(b.tsType)

    def alias[A](name: String)(using u: TsType[A]): TsType[A] =
      instance(TSExpr.TSTypeAlias(name, u.tsType))

    def external[A](tsName: String): TsType[A] =
      instance(TSExpr.TSTypeReference(tsName))

    // ---- Sanely-automatic entry point ----
    inline given derived: [A] => TsType[A] = ${ TsTypeMacros.deriveImpl[A] }

  // ============================================================
  // 4. Macro — with discriminator field & cycle detection
  // ============================================================

  import scala.quoted.*

  object TsTypeMacros:

    def deriveImpl[A: Type](using Quotes): Expr[TsType[A]] =
      import quotes.reflect.*

      val derivedSymbol: Symbol =
        Symbol.requiredMethod("tstype.TsType.derived")

      val visiting = scala.collection.mutable.Set.empty[String]

      def summonOrDerive[T: Type]: Expr[TsType[T]] =
        Expr.summonIgnoring[TsType[T]](derivedSymbol) match
          case Some(userInstance) => userInstance
          case None               => deriveInternal[T]

      def deriveInternal[T: Type]: Expr[TsType[T]] =
        val typeName = Type.show[T]

        if visiting.contains(typeName) then
          report.warning(
            s"Circular reference detected for $typeName. " +
              s"Emitting a type reference. Consider providing an explicit " +
              s"TsType instance via TsType.external[${typeName}](\"I${typeName.split('.').last}\")."
          )
          val shortName = Expr(typeName.split('.').last)
          return '{ TsType.instance[T](TSExpr.TSTypeReference($shortName)) }

        visiting += typeName

        val result = Expr.summon[scala.deriving.Mirror.Of[T]] match
          case Some(mirror) =>
            mirror match
              case '{
                    $mp: scala.deriving.Mirror.ProductOf[T] {
                      type MirroredElemLabels = labels
                      type MirroredElemTypes = elems
                      type MirroredLabel = name
                    }
                  } =>
                deriveProduct[T, labels, elems, name]

              case '{
                    $ms: scala.deriving.Mirror.SumOf[T] {
                      type MirroredElemTypes = elems
                      type MirroredElemLabels = labels
                      type MirroredLabel = name
                    }
                  } =>
                deriveSum[T, elems, labels, name]

              case _ =>
                report.errorAndAbort(s"Cannot derive TsType for $typeName: unsupported Mirror")

          case None =>
            report.errorAndAbort(
              s"Cannot derive TsType for $typeName: no Mirror and no given TsType in scope"
            )

        visiting -= typeName
        result

      // ---- Product → TSInterface ----

      def deriveProduct[T: Type, Labels: Type, Elems: Type, Name: Type]: Expr[TsType[T]] =
        val name = constString[Name]
        val labels = tupleStrings[Labels]
        val fields = tupleFieldExprs[Elems](labels)

        '{
          TsType.instance[T](
            TSExpr.TSInterface(
              ${ Expr(name) },
              $fields
            )
          )
        }

      // ---- Sum → TSUnion with discriminator field ----

      def deriveSum[T: Type, Elems: Type, Labels: Type, Name: Type]: Expr[TsType[T]] =
        val name = constString[Name]
        val childLabels = tupleStrings[Labels]
        val children = sumChildExprs[Elems](childLabels)

        '{ TsType.instance[T](TSExpr.TSUnion($children)) }

      // ---- Helpers ----

      def constString[S: Type]: String =
        TypeRepr.of[S] match
          case ConstantType(StringConstant(s)) => s
          case other                           => Type.show[S]

      def tupleStrings[T: Type]: List[String] =
        Type.of[T] match
          case '[EmptyTuple] => Nil
          case '[h *: t]     => constString[h] :: tupleStrings[t]
          case _             => report.errorAndAbort(s"Unexpected type in tuple: ${Type.show[T]}")

      def tupleFieldExprs[T: Type](labels: List[String]): Expr[List[TSField]] =
        Type.of[T] match
          case '[EmptyTuple] => '{ Nil }
          case '[h *: t]     =>
            val inst = summonOrDerive[h]
            val label = Expr(labels.head)
            val tail = tupleFieldExprs[t](labels.tail)
            '{ TSField($label, $inst.tsType) :: $tail }
          case _ => report.errorAndAbort(s"Unexpected type in tuple: ${Type.show[T]}")

      def sumChildExprs[T: Type](labels: List[String]): Expr[List[TSExpr]] =
        Type.of[T] match
          case '[EmptyTuple] => '{ Nil }
          case '[h *: t]     =>
            val inst = summonOrDerive[h]
            val childName = Expr(labels.head)
            val tail = sumChildExprs[t](labels.tail)
            '{
              val childTpe = $inst.tsType
              val tagged = childTpe match
                case iface: TSExpr.TSInterface =>
                  iface.copy(fields = TSField("type", TSExpr.TSLiteral("\"" + $childName + "\"")) :: iface.fields)
                case other => other
              tagged :: $tail
            }
          case _ => report.errorAndAbort(s"Unexpected type in sum: ${Type.show[T]}")

      summonOrDerive[A]

  // ============================================================
  // 5. DSL — field removal etc.
  // ============================================================

  extension (tpe: TSExpr)
    def -(fieldName: String): TSExpr = tpe match
      case iface: TSExpr.TSInterface =>
        iface.copy(fields = iface.fields.filterNot(_.name == fieldName))
      case other => other

    def +(field: TSField): TSExpr = tpe match
      case iface: TSExpr.TSInterface =>
        iface.copy(fields = iface.fields :+ field)
      case other => other

  // ============================================================
  // 6. Example usage
  // ============================================================

  /*
  package tstype.example

  import tstype.*
  import java.util.UUID
  import java.time.Instant as JInstant

  case class Address(
                      street: String,
                      city: String,
                      zip: String,
                      country: String
                    )

  case class Email(address: String)

  object Email:
    given TsType[Email] = TsType.sameAs[Email, String]

  case class ContactInfo(
                          email: Email,
                          phone: Option[String],
                          addresses: List[Address]
                        )

  case class User(
                   id: UUID,
                   name: String,
                   active: Boolean,
                   contact: ContactInfo,
                   tags: Set[String],
                   metadata: Map[String, String],
                   createdAt: JInstant
                 ) derives TsType

  enum Role derives TsType:
    case Admin
    case Editor(level: Int)
    case Viewer

  case class RoleAssignment(
                             userId: UUID,
                             role: Role,
                             coordinates: (Double, Double)
                           ) derives TsType

  case class ApiResponse(
                          result: Either[String, User]
                        ) derives TsType

  @main def run(): Unit =
    println("// === Generated TypeScript Definitions ===\n")
    println(TSEmitter.emitAll(TsType[User].tsType))
    println()
    println(TSEmitter.emitAll(TsType[Role].tsType))
    println()
    println(TSEmitter.emitAll(TsType[RoleAssignment].tsType))
    println()
    println(TSEmitter.emitAll(TsType[ApiResponse].tsType))

    val userWithoutMeta = TsType[User].tsType - "metadata"
    println("\n// User without metadata:")
    println(TSEmitter.emit(userWithoutMeta))


   */

}

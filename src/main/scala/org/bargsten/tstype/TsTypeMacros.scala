package org.bargsten.tstype

import scala.collection.immutable.ListMap
import scala.quoted.*

object TsTypeMacros:

  def deriveImpl[A: Type](using Quotes): Expr[TsType[A]] =
    import quotes.reflect.*

    val derivedSymbols: List[Symbol] =
      Symbol.requiredModule("org.bargsten.tstype.TsType").methodMember("derived")

    val visiting = scala.collection.mutable.Set.empty[String]

    def summonOrDerive[T: Type]: Expr[TsType[T]] =
      Expr.summonIgnoring[TsType[T]](derivedSymbols*) match
        case Some(userInstance) => userInstance
        case None               => deriveInternal[T]

    def deriveInternal[T: Type]: Expr[TsType[T]] =
      val typeRepr = TypeRepr.of[T]
      val symbol = typeRepr.typeSymbol

      // Simple enum cases (parameterless) share typeSymbol with the parent enum.
      // Use termSymbol to get a unique qualified name.
      val ts = typeRepr.termSymbol
      val fullName =
        if ts != Symbol.noSymbol && ts.flags.is(Flags.Case) then s"${ts.owner.fullName}.${ts.name}"
        else symbol.fullName

      // Cycle detection
      if visiting.contains(fullName) then
        report.warning(
          s"Circular reference for ${Type.show[T]}. " +
            s"Emitting forward reference. Provide an explicit TsType via TsType.external."
        )
        val name = Expr(fullName)
        return '{ TsType[T](TsExpr.TsTypeReference($name)) }

      visiting += fullName

      val result = Expr.summon[scala.deriving.Mirror.Of[T]] match
        case Some(mirror) =>
          mirror match
            case '{
                  $mp: scala.deriving.Mirror.ProductOf[T] {
                    type MirroredElemLabels = labels
                    type MirroredElemTypes = elems
                  }
                } =>
              deriveProduct[T, labels, elems](fullName)

            case '{
                  $ms: scala.deriving.Mirror.SumOf[T] {
                    type MirroredElemTypes = elems
                    type MirroredElemLabels = labels
                  }
                } =>
              deriveSum[T, elems, labels](fullName)

            case _ =>
              report.errorAndAbort(s"Cannot derive TsType for ${Type.show[T]}: unsupported Mirror")

        case None =>
          report.errorAndAbort(
            s"Cannot derive TsType for ${Type.show[T]}: no Mirror and no given TsType in scope"
          )

      visiting -= fullName
      result

    // ---- Product → TsInterface ----

    def deriveProduct[T: Type, Labels: Type, Elems: Type](fullName: String): Expr[TsType[T]] =
      val labels = tupleStrings[Labels]
      val fields = productFields[Elems](labels)
      val qn = Expr(fullName)

      '{ TsType[T](TsExpr.TsInterface($qn, ListMap($fields*))) }

    def productFields[T: Type](labels: List[String]): Expr[Seq[(String, TsExpr)]] =
      Type.of[T] match
        case '[EmptyTuple]     => '{ Seq.empty }
        case '[Option[h] *: t] =>
          val inst = summonOrDerive[h]
          val label = Expr(labels.head)
          val tail = productFields[t](labels.tail)
          '{ ($label, $inst.get | TsExpr.TsUndefined) +: $tail }
        case '[h *: t] =>
          val inst = summonOrDerive[h]
          val label = Expr(labels.head)
          val tail = productFields[t](labels.tail)
          '{ ($label, $inst.get) +: $tail }
        case _ => report.errorAndAbort(s"Cannot derive TsType for ${Type.show[T]}: unsupported Mirror")

    // ---- Sum → TsAlias of TsUnion ----

    def allSimpleCases[T: Type]: Boolean = Type.of[T] match
      case '[EmptyTuple] => true
      case '[h *: t]     =>
        val tr = TypeRepr.of[h]
        val ts = tr.termSymbol
        (ts != Symbol.noSymbol && ts.flags.is(Flags.Case)) && allSimpleCases[t]
      case _ => false

    def deriveSum[T: Type, Elems: Type, Labels: Type](fullName: String): Expr[TsType[T]] =
      val childLabels = tupleStrings[Labels]
      val qn = Expr(fullName)

      if allSimpleCases[Elems] then
        // All cases are parameterless → string literal union
        val literals = Expr.ofSeq(childLabels.map(l => '{ TsExpr.TsLiteralString(${ Expr(l) }) }))
        '{
          val ls = $literals
          val union = ls match
            case Seq()       => TsExpr.TsNever
            case Seq(single) => single
            case multiple    => TsExpr.TsUnion(multiple)
          TsType[T](TsExpr.TsAlias($qn, union))
        }
      else
        val children = sumChildren[Elems](childLabels)
        '{
          val cs = $children
          val union = cs match
            case Seq()       => TsExpr.TsNever
            case Seq(single) => single
            case multiple    => TsExpr.TsUnion(multiple)
          TsType[T](TsExpr.TsAlias($qn, union))
        }

    def sumChildren[T: Type](labels: List[String]): Expr[Seq[TsExpr]] =
      Type.of[T] match
        case '[EmptyTuple] => '{ Seq.empty }
        case '[h *: t]     =>
          val inst = summonOrDerive[h]
          val childName = Expr(labels.head)
          val tail = sumChildren[t](labels.tail)
          '{
            val childTpe = $inst.get
            // Wrap named types in a TsTypeReference with discriminator
            val ref = childTpe match
              case iface: TsExpr.TsInterface =>
                TsExpr.TsTypeReference(iface.qualifiedName, Some(iface), Some($childName))
              case alias: TsExpr.TsAlias =>
                TsExpr.TsTypeReference(alias.qualifiedName, Some(alias), Some($childName))
              case other => other
            ref +: $tail
          }
        case _ => report.errorAndAbort(s"Cannot derive TsType for ${Type.show[T]}: unsupported Mirror")

    // ---- Helpers ----

    def constString[S: Type]: String =
      TypeRepr.of[S] match
        case ConstantType(StringConstant(s)) => s
        case _                               => Type.show[S]

    def tupleStrings[T: Type]: List[String] =
      Type.of[T] match
        case '[EmptyTuple] => Nil
        case '[h *: t]     => constString[h] :: tupleStrings[t]
        case _             => report.errorAndAbort(s"Cannot derive TsType for ${Type.show[T]}: unsupported Mirror")

    deriveInternal[A]

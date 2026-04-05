import scala.collection.immutable.ListMap

object TsEmitter:
  import TsExpr.*

  def emit(tp: TsExpr)(using o: StyleOptions = StyleOptions()): String = tp match
    case TsAny                    => "any"
    case TsBoolean                => "boolean"
    case TsNever                  => "never"
    case TsNull                   => "null"
    case TsNumber                 => "number"
    case TsObject                 => "object"
    case TsString                 => "string"
    case TsUndefined              => "undefined"
    case TsUnknown                => "unknown"
    case TsVoid                   => "void"
    case TsLiteralString(v)       => s""""$v""""
    case TsLiteralNumber(v)       => v.toString
    case TsLiteralBoolean(v)      => v.toString
    case TsArray(e)               => s"${emit(e)}[]"
    case TsTuple(ms)              => ms.map(emit).mkString("[", ", ", "]")
    case TsUnion(Seq())           => "never"
    case TsUnion(Seq(e))          => emit(e)
    case TsUnion(of)              => s"(${of.map(emit).mkString(" | ")})"
    case TsIntersection(of)       => s"(${of.map(emit).mkString(" & ")})"
    case TsIndexedInterface(n,i,v) => s"{ [ $n: ${emit(i)} ]: ${emit(v)}${o.sc} }"
    case TsFunction(args, rt)     => s"${serializeArgs(args)} => ${emit(rt)}"
    case r: TsTypeReference       => r.name
    case i: TsInterface           => i.name
    case a: TsAlias               => a.name
    case e: TsEnum                => e.name

  private def serializeArgs(args: ListMap[String, TsExpr])(using StyleOptions): String =
    args.map((n, t) => s"$n: ${emit(t)}").mkString("(", ", ", ")")

  /** Emit a named type definition */
  def emitNamed(tp: TsExpr)(using o: StyleOptions): Option[String] = tp match
    case TsAlias(_, underlying) =>
      val a = tp.asInstanceOf[TsAlias]
      Some(s"export type ${a.name} = ${emit(underlying)}${o.sc}")

    case TsInterface(_, members) =>
      val iface = tp.asInstanceOf[TsInterface]
      val mbs = members.map { (name, tpe) =>
        tpe match
          case TsUnion(ms) if ms.contains(TsUndefined) =>
            val filtered = TsUnion(ms.filter(_ != TsUndefined))
            s"  $name?: ${emit(filtered)}"
          case other =>
            s"  $name: ${emit(other)}"
      }
      Some(s"export interface ${iface.name} {\n${mbs.mkString(s"${o.sc}\n")}${o.sc}\n}")

    case e: TsEnum =>
      val mbs = e.entries.map {
        case (name, Some(v)) => s"  $name = ${emit(v)}"
        case (name, None)    => s"  $name"
      }
      Some(s"export ${if e.const then "const " else ""}enum ${e.name} {\n${mbs.mkString(",\n")},\n}${o.sc}")

    case _: TsTypeReference => None
    case _                  => None

  /** Discover all named types reachable from a type tree */
  def discoverNamed(tp: TsExpr)(using o: StyleOptions): Set[TsExpr] = tp match
    case u: TsUnion =>
      // Inject discriminator into interface members of union children
      val transformed = u.of.map {
        case TsTypeReference(qn, Some(TsInterface(iqn, members)), Some(disc)) =>
          val withDisc = o.taggedUnionDiscriminator match
            case Some(field) => members + (field -> TsLiteralString(disc))
            case None        => members
          TsTypeReference(qn, Some(TsInterface(iqn, withDisc)), Some(disc))
        case other => other
      }
      transformed.toSet.flatMap(discoverNamed) ++ namedSet(tp)
    case TsArray(e)                   => discoverNamed(e) ++ namedSet(tp)
    case TsTuple(es)                  => es.toSet.flatMap(discoverNamed) ++ namedSet(tp)
    case TsIntersection(of)           => of.toSet.flatMap(discoverNamed) ++ namedSet(tp)
    case TsIndexedInterface(_, _, v)  => discoverNamed(v) ++ namedSet(tp)
    case TsFunction(args, rt)         => args.values.toSet.flatMap(discoverNamed) ++ discoverNamed(rt) ++ namedSet(tp)
    case TsInterface(_, members)      => members.values.toSet.flatMap(discoverNamed) ++ namedSet(tp)
    case TsAlias(_, underlying)       => discoverNamed(underlying) ++ namedSet(tp)
    case TsTypeReference(_, impl, _) => impl.toSet.flatMap(discoverNamed) ++ namedSet(tp)
    case _                            => namedSet(tp)

  private def namedSet(tp: TsExpr): Set[TsExpr] = tp match
    case _: TsInterface     => Set(tp)
    case _: TsAlias         => Set(tp)
    case _: TsEnum          => Set(tp)
    case _: TsTypeReference => Set(tp)
    case _                  => Set.empty

  def emitAll(tpe: TsExpr)(using o: StyleOptions = StyleOptions()): String =
    discoverNamed(tpe)
      .collect {
        case TsTypeReference(_, Some(impl), _) => impl
        case o if !o.isInstanceOf[TsTypeReference] => o
      }
      .toSeq
      .flatMap(emitNamed)
      .mkString("\n\n") + "\n"


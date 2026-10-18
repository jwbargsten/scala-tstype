package org.bargsten.tstype

import org.bargsten.tstype.TsExpr.*

import scala.collection.immutable.ListMap

object TsEmitter:
  import TsExpr.*

  def emit(tp: TsExpr)(using o: StyleOptions = StyleOptions()): String = tp match
    case TsAny                       => "any"
    case TsBoolean                   => "boolean"
    case TsNever                     => "never"
    case TsNull                      => "null"
    case TsNumber                    => "number"
    case TsObject                    => "object"
    case TsString                    => "string"
    case TsUndefined                 => "undefined"
    case TsUnknown                   => "unknown"
    case TsVoid                      => "void"
    case TsLiteralString(v)          => quote(v)
    case TsLiteralNumber(v)          => v.toString
    case TsLiteralBoolean(v)         => v.toString
    case TsArray(e)                  => s"${emit(e)}[]"
    case TsTuple(ms)                 => ms.map(emit).mkString("[", ", ", "]")
    case TsUnion(Seq())              => "never"
    case TsUnion(Seq(e))             => emit(e)
    case TsUnion(of)                 => s"(${of.map(emit).mkString(" | ")})"
    case TsIntersection(of)          => s"(${of.map(emit).mkString(" & ")})"
    case TsIndexedInterface(n, i, v) =>
      if isIndexSignatureKey(i) then s"{ [ $n: ${emit(i)} ]: ${emit(v)}${o.sc} }"
      else s"Partial<Record<${emit(i)}, ${emit(v)}>>"
    case TsFunction(args, rt)                                     => s"${serializeArgs(args)} => ${emit(rt)}"
    case fn: TsFunctionNamed                                      => s"typeof ${fn.name}"
    case r @ TsTypeReference(_, Some(_: TsInterface), Some(disc)) =>
      o.taggedUnionDiscriminator.fold(r.name)(field => s"(${r.name} & { ${property(field)}: ${quote(disc)}${o.sc} })")
    case r: TsTypeReference     => r.name
    case i: TsInterface         => i.name
    case a: TsAlias             => a.name
    case e: TsEnum              => e.name
    case ii: TsInterfaceIndexed => ii.name

  private val Identifier = "[A-Za-z_$][A-Za-z0-9_$]*".r

  private def property(name: String): String = if Identifier.matches(name) then name else quote(name)

  private def quote(s: String): String =
    val sb = StringBuilder("\"")
    s.foreach {
      case '"'          => sb.append("\\\"")
      case '\\'         => sb.append("\\\\")
      case '\n'         => sb.append("\\n")
      case '\r'         => sb.append("\\r")
      case '\t'         => sb.append("\\t")
      case c if c < ' ' => sb.append(f"\\u${c.toInt}%04x")
      case c            => sb.append(c)
    }
    sb.append('"').toString

  private def serializeArgs(args: ListMap[String, TsExpr])(using StyleOptions): String =
    args.map((n, t) => s"$n: ${emit(t)}").mkString("(", ", ", ")")

  // TypeScript only allows `string`, `number`, `symbol`, or template literal types as index signature keys.
  private def isIndexSignatureKey(tp: TsExpr): Boolean = tp match
    case TsString | TsNumber => true
    case _                   => false

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
            s"  ${property(name)}?: ${emit(filtered)}"
          case other =>
            s"  ${property(name)}: ${emit(other)}"
      }
      Some(s"export interface ${iface.name} {\n${mbs.mkString(s"${o.sc}\n")}${o.sc}\n}")

    case e: TsEnum =>
      val mbs = e.entries.map {
        case (name, Some(v)) => s"  ${property(name)} = ${emit(v)}"
        case (name, None)    => s"  ${property(name)}"
      }
      Some(s"export ${if e.const then "const " else ""}enum ${e.name} {\n${mbs.mkString(",\n")},\n}${o.sc}")

    case ii: TsInterfaceIndexed =>
      Some(s"export interface ${ii.name} {\n  [ ${ii.indexName}: ${emit(ii.indexType)} ]: ${emit(ii.valueType)}${o.sc}\n}")

    case fn: TsFunctionNamed =>
      Some(s"export function ${fn.name}${serializeArgs(fn.signature.arguments)}: ${emit(fn.signature.returnType)}${o.sc}")

    case _: TsTypeReference => None
    case _                  => None

  /** Discover all named types reachable from a type tree */
  def discoverNamed(tp: TsExpr)(using StyleOptions): Set[TsExpr] = discover(tp).toSet

  // depth-first, each named type before the types it references
  private def discover(tp: TsExpr): Seq[TsExpr] =
    val children = tp match
      case TsUnion(of)                    => of
      case TsArray(e)                     => Seq(e)
      case TsTuple(es)                    => es
      case TsIntersection(of)             => of
      case TsIndexedInterface(_, i, v)    => Seq(i, v)
      case TsInterfaceIndexed(_, _, i, v) => Seq(i, v)
      case TsFunction(args, rt)           => args.values.toSeq :+ rt
      case TsFunctionNamed(_, sig)        => sig.arguments.values.toSeq :+ sig.returnType
      case TsInterface(_, members)        => members.values.toSeq
      case TsAlias(_, underlying)         => Seq(underlying)
      case TsTypeReference(_, impl, _)    => impl.toSeq
      case _                              => Seq.empty
    named(tp) ++ children.flatMap(discover)

  private def named(tp: TsExpr): Seq[TsExpr] = tp match
    case _: TsInterface | _: TsAlias | _: TsEnum | _: TsTypeReference | _: TsInterfaceIndexed | _: TsFunctionNamed => Seq(tp)
    case _                                                                                                         => Seq.empty

  def emitAll(tpe: TsExpr)(using o: StyleOptions = StyleOptions()): String =
    emitAll(Seq(tpe))

  /** Declarations in the order of `tpes`, each followed by the types it references (depth-first). */
  def emitAll(tpes: Seq[TsExpr])(using o: StyleOptions): String =
    val discovered = tpes.flatMap(discover).distinct
    val renameMap = renames(discovered)
    val declarations = discovered
      .map(renameInType(renameMap, _))
      .collect {
        case TsTypeReference(_, Some(impl), _)     => impl
        case o if !o.isInstanceOf[TsTypeReference] => o
      }
      .distinct
    for (name, ds) <- declarations.groupBy(_.name) if ds.size > 1 do
      throw IllegalArgumentException(
        s"${ds.size} different declarations named $name (e.g. a generic case class):\n${ds.flatMap(emitNamed).mkString("\n")}"
      )
    declarations
      .flatMap(emitNamed)
      .mkString("\n\n") + "\n"

  /** Resolve name clashes among named types by prepending package segments to disambiguate.
    *
    * For types with the same `name` but different `qualifiedName`:
    *   - org.bargsten.fsrs.Card -> FsrsCard
    *   - org.bargsten.learn.Card -> LearnCard
    */
  def resolveNameClashes(types: Set[TsExpr]): Set[TsExpr] =
    val renameMap = renames(types)
    if renameMap.isEmpty then types
    else types.map(renameInType(renameMap, _))

  private def renames(types: Iterable[TsExpr]): Map[String, String] =
    types
      .filter(isNamed)
      .groupBy(_.name)
      .values
      .flatMap { group =>
        val distinctByQN = group.toSeq.map(qualifiedNameOf).distinct
        if distinctByQN.size <= 1 then Seq.empty
        else disambiguate(distinctByQN)
      }
      .toMap

  private def qualifiedNameOf(tp: TsExpr): String = tp match
    case i: TsInterface         => i.qualifiedName
    case a: TsAlias             => a.qualifiedName
    case r: TsTypeReference     => r.qualifiedName
    case e: TsEnum              => e.qualifiedName
    case ii: TsInterfaceIndexed => ii.qualifiedName
    case fn: TsFunctionNamed    => fn.qualifiedName
    case _                      => ""

  private def isNamed(tp: TsExpr): Boolean = tp match
    case TsTypeReference(_, None, _) => false // forward references don't produce output
    case _: TsInterface | _: TsAlias | _: TsTypeReference | _: TsEnum | _: TsInterfaceIndexed | _: TsFunctionNamed => true
    case _                                                                                                         => false

  private def disambiguate(qualifiedNames: Seq[String]): Seq[(String, String)] =
    val segments = qualifiedNames.map(qn => qn -> TsExpr.extractNameParts(qn))
    val maxDepth = segments.map(_._2.size - 1).min

    def tryDepth(depth: Int): Seq[(String, String)] =
      val proposed = segments.map { case (qn, segs) =>
        val className = segs.last
        val pkgSegments = segs.dropRight(1).takeRight(depth)
        val newName = pkgSegments.map(s => s"${s.head.toUpper}${s.tail}").mkString + className
        (qn, newName)
      }
      if proposed.map(_._2).distinct.size == proposed.size then proposed
      else if depth < maxDepth then tryDepth(depth + 1)
      else proposed

    tryDepth(1)

  private def renameInType(renameMap: Map[String, String], tp: TsExpr): TsExpr = tp match
    case TsTypeReference(qn, impl, disc) =>
      val newImpl = impl.map(renameInType(renameMap, _))
      val newQn = impl
        .flatMap {
          case i: TsInterface => renameMap.get(i.qualifiedName)
          case a: TsAlias     => renameMap.get(a.qualifiedName)
          case e: TsEnum      => renameMap.get(e.qualifiedName)
          case _              => None
        }
        .orElse(renameMap.get(qn)) // forward reference of a recursive type: no impl
        .getOrElse(qn)
      TsTypeReference(newQn, newImpl, disc)
    case TsInterface(qn, members) =>
      val newQn = renameMap.getOrElse(qn, qn)
      TsInterface(newQn, members.map((k, v) => (k, renameInType(renameMap, v))))
    case TsAlias(qn, underlying) =>
      val newQn = renameMap.getOrElse(qn, qn)
      TsAlias(newQn, renameInType(renameMap, underlying))
    case TsEnum(qn, c, entries) =>
      TsEnum(renameMap.getOrElse(qn, qn), c, entries)
    case TsArray(e)                      => TsArray(renameInType(renameMap, e))
    case TsUnion(of)                     => TsUnion(of.map(renameInType(renameMap, _)))
    case TsTuple(of)                     => TsTuple(of.map(renameInType(renameMap, _)))
    case TsIntersection(of)              => TsIntersection(of.map(renameInType(renameMap, _)))
    case TsFunction(args, rt)            => TsFunction(args.map((k, v) => (k, renameInType(renameMap, v))), renameInType(renameMap, rt))
    case TsIndexedInterface(n, i, v)     => TsIndexedInterface(n, renameInType(renameMap, i), renameInType(renameMap, v))
    case TsInterfaceIndexed(qn, n, i, v) =>
      TsInterfaceIndexed(renameMap.getOrElse(qn, qn), n, renameInType(renameMap, i), renameInType(renameMap, v))
    case TsFunctionNamed(qn, sig) => TsFunctionNamed(renameMap.getOrElse(qn, qn), renameInType(renameMap, sig).asInstanceOf[TsFunction])
    case other                    => other

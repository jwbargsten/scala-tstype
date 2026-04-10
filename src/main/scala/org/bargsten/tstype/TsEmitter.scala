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
    case TsLiteralString(v)          => s""""$v""""
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
    case TsFunction(args, rt)   => s"${serializeArgs(args)} => ${emit(rt)}"
    case fn: TsFunctionNamed    => s"typeof ${fn.name}"
    case r: TsTypeReference     => r.name
    case i: TsInterface         => i.name
    case a: TsAlias             => a.name
    case e: TsEnum              => e.name
    case ii: TsInterfaceIndexed => ii.name

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

    case ii: TsInterfaceIndexed =>
      Some(s"export interface ${ii.name} {\n  [ ${ii.indexName}: ${emit(ii.indexType)} ]: ${emit(ii.valueType)}${o.sc}\n}")

    case fn: TsFunctionNamed =>
      Some(s"export function ${fn.name}${serializeArgs(fn.signature.arguments)}: ${emit(fn.signature.returnType)}${o.sc}")

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
    case TsArray(e)                     => discoverNamed(e) ++ namedSet(tp)
    case TsTuple(es)                    => es.toSet.flatMap(discoverNamed) ++ namedSet(tp)
    case TsIntersection(of)             => of.toSet.flatMap(discoverNamed) ++ namedSet(tp)
    case TsIndexedInterface(_, i, v)    => discoverNamed(i) ++ discoverNamed(v) ++ namedSet(tp)
    case TsInterfaceIndexed(_, _, i, v) => discoverNamed(i) ++ discoverNamed(v) ++ namedSet(tp)
    case TsFunction(args, rt)           => args.values.toSet.flatMap(discoverNamed) ++ discoverNamed(rt) ++ namedSet(tp)
    case TsFunctionNamed(_, sig)     => sig.arguments.values.toSet.flatMap(discoverNamed) ++ discoverNamed(sig.returnType) ++ namedSet(tp)
    case TsInterface(_, members)     => members.values.toSet.flatMap(discoverNamed) ++ namedSet(tp)
    case TsAlias(_, underlying)      => discoverNamed(underlying) ++ namedSet(tp)
    case TsTypeReference(_, impl, _) => impl.toSet.flatMap(discoverNamed) ++ namedSet(tp)
    case _                           => namedSet(tp)

  private def namedSet(tp: TsExpr): Set[TsExpr] = tp match
    case _: TsInterface        => Set(tp)
    case _: TsAlias            => Set(tp)
    case _: TsEnum             => Set(tp)
    case _: TsTypeReference    => Set(tp)
    case _: TsInterfaceIndexed => Set(tp)
    case _: TsFunctionNamed    => Set(tp)
    case _                     => Set.empty

  def emitAll(tpe: TsExpr)(using o: StyleOptions = StyleOptions()): String =
    emitAll(Seq(tpe))

  def emitAll(tpes: Seq[TsExpr])(using o: StyleOptions): String =
    val discovered = tpes.toSet.flatMap(discoverNamed)
    val resolved = resolveNameClashes(discovered)
    resolved
      .collect {
        case TsTypeReference(_, Some(impl), _)     => impl
        case o if !o.isInstanceOf[TsTypeReference] => o
      }
      .toSeq
      .flatMap(emitNamed)
      .mkString("\n\n") + "\n"

  /** Resolve name clashes among named types by prepending package segments to disambiguate.
    *
    * For types with the same `name` but different `qualifiedName`:
    *   - org.bargsten.fsrs.Card -> FsrsCard
    *   - org.bargsten.learn.Card -> LearnCard
    */
  def resolveNameClashes(types: Set[TsExpr]): Set[TsExpr] =
    val named = types.filter(isNamed)
    val grouped = named.groupBy(_.name)
    val renameMap: Map[String, String] = grouped.values.flatMap { group =>
      val distinctByQN = group.toSeq.map(qualifiedNameOf).distinct
      if distinctByQN.size <= 1 then Seq.empty
      else disambiguate(distinctByQN)
    }.toMap

    if renameMap.isEmpty then types
    else types.map(renameInType(renameMap, _))

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
    val segments = qualifiedNames.map(_.split('.').toSeq)
    val maxDepth = segments.map(_.size - 1).min

    def tryDepth(depth: Int): Seq[(String, String)] =
      val proposed = segments.map { segs =>
        val className = segs.last
        val pkgSegments = segs.dropRight(1).takeRight(depth)
        val newName = pkgSegments.map(s => s"${s.head.toUpper}${s.tail}").mkString + className
        (segs.mkString("."), newName)
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

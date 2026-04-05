import scala.collection.immutable.ListMap

enum TsExpr:
  case TsAny
  case TsBoolean
  case TsNever
  case TsNull
  case TsNumber
  case TsObject
  case TsString
  case TsUndefined
  case TsUnknown
  case TsVoid
  case TsLiteralString(value: String)
  case TsLiteralNumber(value: BigDecimal)
  case TsLiteralBoolean(value: Boolean)
  case TsArray(elementType: TsExpr)
  case TsTuple(of: Seq[TsExpr])
  case TsUnion(of: Seq[TsExpr])
  case TsIntersection(of: Seq[TsExpr])
  case TsIndexedInterface(indexName: String = "key", indexType: TsExpr, valueType: TsExpr)
  case TsFunction(arguments: ListMap[String, TsExpr] = ListMap(), returnType: TsExpr = TsVoid)
  case TsInterface(qualifiedName: String, members: ListMap[String, TsExpr])
  case TsAlias(qualifiedName: String, underlying: TsExpr)
  case TsTypeReference(qualifiedName: String, impl: Option[TsExpr] = None, discriminator: Option[String] = None)
  case TsEnum(qualifiedName: String, const: Boolean, entries: ListMap[String, Option[TsExpr]])

  def |(other: TsExpr): TsExpr.TsUnion = TsExpr.TsUnion.of(this, other)
  def array: TsExpr.TsArray = TsExpr.TsArray(this)

  def name: String = this match
    case i: TsInterface     => TsExpr.extractName(i.qualifiedName)
    case a: TsAlias         => TsExpr.extractName(a.qualifiedName)
    case r: TsTypeReference => TsExpr.extractName(r.qualifiedName)
    case e: TsEnum          => TsExpr.extractName(e.qualifiedName)
    case _                  => ""

object TsExpr:
  object TsTuple:
    def of(of: TsExpr*): TsExpr.TsTuple = TsExpr.TsTuple(of)

  object TsUnion:
    def of(of: TsExpr*): TsExpr.TsUnion = TsExpr.TsUnion(of).flatten

  object TsEnum:
    def of(qualifiedName: String, entries: String*): TsExpr.TsEnum =
      TsExpr.TsEnum(qualifiedName, const = false, ListMap(entries.map(e => e -> None)*))
    def string(qualifiedName: String, entries: (String, String)*): TsExpr.TsEnum =
      TsExpr.TsEnum(qualifiedName, const = false, ListMap(entries.map((e, v) => e -> Some(TsExpr.TsLiteralString(v)))*))

  extension (u: TsExpr.TsUnion)
    def flatten: TsExpr.TsUnion =
      if u.of.exists(_.isInstanceOf[TsExpr.TsUnion]) then
        TsExpr.TsUnion(u.of.flatMap {
          case nested: TsExpr.TsUnion => nested.flatten.of
          case other                  => Seq(other)
        })
      else u

  private def extractName(qualifiedName: String): String =
    qualifiedName.replace("$.", ".").split('.').last.stripPrefix("_$").stripSuffix("$")

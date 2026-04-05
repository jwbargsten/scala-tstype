import TsExpr.*

trait TsType[A]:
  def get: TsExpr
  override def toString: String = s"TsType($get)"

  def |(other: TsExpr): TsUnion = get | other
  def |(other: TsType[?]): TsUnion = get | other.get

object TsType:
  def apply[A](tt: TsExpr): TsType[A] = new TsType[A] { val get: TsExpr = tt }

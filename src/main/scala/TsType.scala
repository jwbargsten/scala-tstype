import TsExpr.*

trait TsType[A]:
  def get: TsExpr
  override def toString: String = s"TsType($get)"

  def |(other: TsExpr): TsUnion = get | other
  def |(other: TsType[?]): TsUnion = get | other.get

object TsType extends TsTypeDefaults:
  def apply[A](tt: TsExpr): TsType[A] = new TsType[A] { val get: TsExpr = tt }

import scala.annotation.unused

trait TsTypeDefaults:

  // ---- Scala primitives ----
  given TsType[Any] = TsType(TsAny)
  given TsType[Unit] = TsType(TsVoid)
  given TsType[Boolean] = TsType(TsBoolean)
  given TsType[String] = TsType(TsString)

  // Numeric[T] covers Int, Long, Double, Float, Short, Byte, BigDecimal, BigInt
  given numericTs: [T: Numeric] => TsType[T] = TsType(TsNumber)

  // Char is numeric in Scala
  given TsType[Char] = TsType(TsNumber)

  // ---- Option / Either ----
  given optionTs: [E: TsType] => TsType[Option[E]] = TsType(summon[TsType[E]].get | TsUndefined)
  given TsType[None.type] = TsType(TsNull)
  given someTs: [E: TsType] => TsType[Some[E]] = TsType(summon[TsType[E]].get)

  given eitherTs: [L: TsType, R: TsType] => TsType[Either[L, R]] =
    TsType(summon[TsType[L]].get | summon[TsType[R]].get)

  // ---- Collections ----
  // Map[String, V] → indexed interface (idiomatic Ts)
  given stringMapTs: [V: TsType] => TsType[Map[String, V]] =
    TsType(TsIndexedInterface(indexType = TsString, valueType = summon[TsType[V]].get))

  // Map[Int, V] → indexed interface
  given intMapTs: [V: TsType] => TsType[Map[Int, V]] =
    TsType(TsIndexedInterface(indexType = TsNumber, valueType = summon[TsType[V]].get))

  // Any Iterable[E] → E[]
  given iterableTs: [E, F[_]] => (e: TsType[E]) => (F[E] <:< Iterable[E]) => TsType[F[E]] =
    TsType(e.get.array)

  // ---- Java types ----
  given TsType[Object] = TsType(TsObject)
  given TsType[Void] = TsType(TsVoid)
  given TsType[java.util.UUID] = TsType(TsString)
  given TsType[java.net.URI] = TsType(TsString)
  given TsType[java.net.URL] = TsType(TsString)

  // java.time.* → string (ISO8601 serialization)
  given temporalTs: [T <: java.time.temporal.Temporal] => TsType[T] = TsType(TsString)
  given TsType[java.util.Date] = TsType(TsString)

  // java.lang.Number subtypes
  given javaNumberTs: [T <: java.lang.Number] => TsType[T] = TsType(TsNumber)

  // java.util.Collection → array
  given javaCollectionTs: [E, F[_]] => (e: TsType[E]) => (F[E] <:< java.util.Collection[E]) => TsType[F[E]] =
    TsType(e.get.array)

  // ---- Tuples ----
  given tuple2: [A: TsType, B: TsType] => TsType[(A, B)] =
    TsType(TsTuple.of(summon[TsType[A]].get, summon[TsType[B]].get))
  given tuple3: [A: TsType, B: TsType, C: TsType] => TsType[(A, B, C)] =
    TsType(TsTuple.of(summon[TsType[A]].get, summon[TsType[B]].get, summon[TsType[C]].get))
  given tuple4: [A: TsType, B: TsType, C: TsType, D: TsType] => TsType[(A, B, C, D)] =
    TsType(TsTuple.of(summon[TsType[A]].get, summon[TsType[B]].get, summon[TsType[C]].get, summon[TsType[D]].get))
  given tuple5: [A: TsType, B: TsType, C: TsType, D: TsType, E: TsType] => TsType[(A, B, C, D, E)] =
    TsType(TsTuple.of(summon[TsType[A]].get, summon[TsType[B]].get, summon[TsType[C]].get, summon[TsType[D]].get, summon[TsType[E]].get))
  given tuple6: [A: TsType, B: TsType, C: TsType, D: TsType, E0: TsType, F: TsType] => TsType[(A, B, C, D, E0, F)] =
    TsType(TsTuple.of(summon[TsType[A]].get, summon[TsType[B]].get, summon[TsType[C]].get, summon[TsType[D]].get, summon[TsType[E0]].get, summon[TsType[F]].get))

  // ---- Literal types ----
  given literalTrue: TsType[true] = TsType(TsLiteralBoolean(true))
  given literalFalse: TsType[false] = TsType(TsLiteralBoolean(false))
  given literalString: [T <: Singleton & String: ValueOf] => TsType[T] = TsType(TsLiteralString(valueOf[T]))
  given literalInt: [T <: Singleton & Int: ValueOf] => TsType[T] = TsType(TsLiteralNumber(BigDecimal(valueOf[T])))

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
  given numericTs[T](using @unused ev: Numeric[T]): TsType[T] = TsType(TsNumber)

  // Char is numeric in Scala
  given TsType[Char] = TsType(TsNumber)

  // ---- Option / Either ----
  given optionTs[E](using e: TsType[E]): TsType[Option[E]] = TsType(e.get | TsUndefined)
  given TsType[None.type] = TsType(TsNull)
  given someTs[E](using e: TsType[E]): TsType[Some[E]] = TsType(e.get)

  given eitherTs[L, R](using l: TsType[L], r: TsType[R]): TsType[Either[L, R]] =
    TsType(l.get | r.get)

  // ---- Collections ----
  // Map[String, V] → indexed interface (idiomatic Ts)
  given stringMapTs[V](using v: TsType[V]): TsType[Map[String, V]] =
    TsType(TsIndexedInterface(indexType = TsString, valueType = v.get))

  // Map[Int, V] → indexed interface
  given intMapTs[V](using v: TsType[V]): TsType[Map[Int, V]] =
    TsType(TsIndexedInterface(indexType = TsNumber, valueType = v.get))

  // Any Iterable[E] → E[]
  given iterableTs[E, F[_]](using e: TsType[E], @unused ev: F[E] <:< Iterable[E]): TsType[F[E]] =
    TsType(e.get.array)

  // ---- Java types ----
  given TsType[Object] = TsType(TsObject)
  given TsType[Void] = TsType(TsVoid)
  given TsType[java.util.UUID] = TsType(TsString)
  given TsType[java.net.URI] = TsType(TsString)
  given TsType[java.net.URL] = TsType(TsString)

  // java.time.* → string (ISO8601 serialization)
  given [T <: java.time.temporal.Temporal]: TsType[T] = TsType(TsString)
  given TsType[java.util.Date] = TsType(TsString)

  // java.lang.Number subtypes
  given [T <: java.lang.Number]: TsType[T] = TsType(TsNumber)

  // java.util.Collection → array
  given javaCollectionTs[E, F[_]](using e: TsType[E], @unused ev: F[E] <:< java.util.Collection[E]): TsType[F[E]] =
    TsType(e.get.array)

  // ---- Tuples ----
  given tuple2[A, B](using a: TsType[A], b: TsType[B]): TsType[(A, B)] =
    TsType(TsTuple.of(a.get, b.get))
  given tuple3[A, B, C](using a: TsType[A], b: TsType[B], c: TsType[C]): TsType[(A, B, C)] =
    TsType(TsTuple.of(a.get, b.get, c.get))
  given tuple4[A, B, C, D](using a: TsType[A], b: TsType[B], c: TsType[C], d: TsType[D]): TsType[(A, B, C, D)] =
    TsType(TsTuple.of(a.get, b.get, c.get, d.get))
  given tuple5[A, B, C, D, E](using a: TsType[A], b: TsType[B], c: TsType[C], d: TsType[D], e: TsType[E]): TsType[(A, B, C, D, E)] =
    TsType(TsTuple.of(a.get, b.get, c.get, d.get, e.get))
  given tuple6[A, B, C, D, E0, F](using
      a: TsType[A],
      b: TsType[B],
      c: TsType[C],
      d: TsType[D],
      e: TsType[E0],
      f: TsType[F]
  ): TsType[(A, B, C, D, E0, F)] =
    TsType(TsTuple.of(a.get, b.get, c.get, d.get, e.get, f.get))

  // ---- Literal types ----
  given TsType[true] = TsType(TsLiteralBoolean(true))
  given TsType[false] = TsType(TsLiteralBoolean(false))
  given literalString[T <: Singleton & String: ValueOf]: TsType[T] = TsType(TsLiteralString(valueOf[T]))
  given literalInt[T <: Singleton & Int: ValueOf]: TsType[T] = TsType(TsLiteralNumber(BigDecimal(valueOf[T])))

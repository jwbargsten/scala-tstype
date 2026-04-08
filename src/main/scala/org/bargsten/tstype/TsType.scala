package org.bargsten.tstype

import org.bargsten.tstype.TsExpr.*
import scala.reflect.ClassTag

trait TsType[A]:
  def get: TsExpr
  override def toString: String = s"TsType($get)"

  def |(other: TsExpr): TsUnion = get | other
  def |(other: TsType[?]): TsUnion = get | other.get

object TsType extends TsTypeDefaults:
  def apply[A](tt: TsExpr): TsType[A] = new TsType[A] { val get: TsExpr = tt }

  def getT[T](using t: TsType[T]): TsType[T] = t
  def external[T](name: String): TsType[T] = TsType(TsExpr.TsTypeReference(name))
  def sameAs[Source, Target](using t: TsType[Target]): TsType[Source] = TsType(t.get)
  inline def derive[T]: TsType[T] = ${ TsTypeMacros.deriveImpl[T] }

  inline def derived[A](using inline m: scala.deriving.Mirror.Of[A]): TsType[A] =
    ${ TsTypeMacros.deriveImpl[A] }

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
  // Map[K, V] → indexed interface using K's TsType (supports opaque types aliasing String/Int)
  given mapTs: [K: TsType, V: TsType] => TsType[Map[K, V]] =
    TsType(TsIndexedInterface(indexType = summon[TsType[K]].get, valueType = summon[TsType[V]].get))

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

  // java.lang.Enum → string literal union
  given javaEnumTs: [E <: java.lang.Enum[E]: ClassTag] => TsType[E] = {
    val cls = summon[ClassTag[E]].runtimeClass
    val values = cls.getEnumConstants.asInstanceOf[Array[E]].toSeq
    TsType(TsAlias(cls.getSimpleName, TsUnion(values.map(v => TsLiteralString(v.name())))))
  }

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
    TsType(
      TsTuple.of(
        summon[TsType[A]].get,
        summon[TsType[B]].get,
        summon[TsType[C]].get,
        summon[TsType[D]].get,
        summon[TsType[E0]].get,
        summon[TsType[F]].get
      )
    )

  // ---- Literal types ----
  given literalTrue: TsType[true] = TsType(TsLiteralBoolean(true))
  given literalFalse: TsType[false] = TsType(TsLiteralBoolean(false))
  given literalString: [T <: Singleton & String: ValueOf] => TsType[T] = TsType(TsLiteralString(valueOf[T]))
  given literalInt: [T <: Singleton & Int: ValueOf] => TsType[T] = TsType(TsLiteralNumber(BigDecimal(valueOf[T])))

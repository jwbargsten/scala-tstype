package org.bargsten.tstype

import TsExpr.*

class DefaultTsTypeTest extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "s")

  test("Tuple2 produces correct TsTuple") {
    case class T(v: (Int, String)) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("v"), TsTuple.of(TsNumber, TsString))
  }

  test("Tuple3 produces correct TsTuple") {
    case class T(v: (Int, String, Double)) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("v"), TsTuple.of(TsNumber, TsString, TsNumber))
  }

  test("Option maps to T | undefined") {
    case class T(v: Option[Int]) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("v"), TsUnion.of(TsNumber, TsUndefined))
  }

  test("Either maps to L | R") {
    case class T(v: Either[Int, String]) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("v"), TsUnion.of(TsNumber, TsString))
  }

  test("Map[String, V] produces indexed interface") {
    case class T(v: Map[String, Int]) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("v"), TsIndexedInterface(indexType = TsString, valueType = TsNumber))
  }

  test("Map[Int, V] produces indexed interface") {
    case class T(v: Map[Int, String]) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("v"), TsIndexedInterface(indexType = TsNumber, valueType = TsString))
  }

  test("Seq types produce arrays") {
    case class T(
        seq: Seq[String],
        list: List[Int],
        vector: Vector[Boolean],
        indexed: IndexedSeq[Int]
    ) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("seq"), TsArray(TsString))
    assertEquals(iface.members("list"), TsArray(TsNumber))
    assertEquals(iface.members("vector"), TsArray(TsBoolean))
    assertEquals(iface.members("indexed"), TsArray(TsNumber))
  }

  test("Set types produce arrays") {
    case class T(v: Set[Int]) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("v"), TsArray(TsNumber))
  }

  test("nested Seq produces nested arrays") {
    case class T(v: Seq[Seq[Int]]) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("v"), TsArray(TsArray(TsNumber)))
  }

  test("java.util.Collection types produce arrays") {
    case class T(
        col: java.util.Collection[Int],
        list: java.util.List[Int],
        arrayList: java.util.ArrayList[Int]
    ) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("col"), TsArray(TsNumber))
    assertEquals(iface.members("list"), TsArray(TsNumber))
    assertEquals(iface.members("arrayList"), TsArray(TsNumber))
  }

  test("java.time types map to string") {
    case class T(
        instant: java.time.Instant,
        localDate: java.time.LocalDate,
        localDateTime: java.time.LocalDateTime
    ) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("instant"), TsString)
    assertEquals(iface.members("localDate"), TsString)
    assertEquals(iface.members("localDateTime"), TsString)
  }

  test("java.util.UUID maps to string") {
    case class T(v: java.util.UUID) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("v"), TsString)
  }

  test("java.net.URI and URL map to string") {
    case class T(uri: java.net.URI, url: java.net.URL) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("uri"), TsString)
    assertEquals(iface.members("url"), TsString)
  }

  test("Java enum maps to string literal union") {
    val ts = summon[TsType[TestJavaEnum]].get
    assertEquals(
      ts,
      TsAlias("TestJavaEnum", TsUnion.of(TsLiteralString("ABC"), TsLiteralString("DEF"), TsLiteralString("GHI")))
    )
  }

  test("sameAs reuses target type representation") {
    class MyId
    given TsType[MyId] = TsType.sameAs[MyId, String]
    assertEquals(summon[TsType[MyId]].get, TsString)
  }

  test("literal types in case class fields") {
    case class T(
        a: "Hello!",
        b: 42,
        c: true
    ) derives TsType
    val iface = summon[TsType[T]].get.asInstanceOf[TsInterface]
    assertEquals(iface.members("a"), TsLiteralString("Hello!"))
    assertEquals(iface.members("b"), TsLiteralNumber(BigDecimal(42)))
    assertEquals(iface.members("c"), TsLiteralBoolean(true))
  }

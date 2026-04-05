package org.bargsten.tstype

import scala.collection.immutable.ListMap
import TsExpr.*

enum Rating(val value: Int) extends Enum[Rating] derives TsType:
  case Again extends Rating(1)
  case Hard extends Rating(2)
  case Good extends Rating(3)
  case Easy extends Rating(4)

class TsEmitterSerializerTest extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "s")

  test("serialize to a simple interface") {
    case class Person(name: String, age: Int) derives TsType
    val typescript = TsEmitter.emitAll(summon[TsType[Person]].get)
    assertEquals(
      typescript,
      """export interface Person {
        |  name: string
        |  age: number
        |}
        |""".stripMargin
    )
  }

  test("serialize an interface with semicolons if configured") {
    given StyleOptions = StyleOptions(semicolons = true)
    case class Person(name: String, age: Int) derives TsType
    val typescript = TsEmitter.emitAll(summon[TsType[Person]].get)
    assertEquals(
      typescript,
      """export interface Person {
        |  name: string;
        |  age: number;
        |}
        |""".stripMargin
    )
  }

  test("nested case classes") {
    case class NestedCaseClass(name: String) derives TsType
    case class ComplexCaseClass(nested: NestedCaseClass) derives TsType
    val typescript = TsEmitter.emitAll(summon[TsType[ComplexCaseClass]].get)
    assert(typescript.contains("export interface ComplexCaseClass"))
    assert(typescript.contains("nested: NestedCaseClass"))
    assert(typescript.contains("export interface NestedCaseClass"))
    assert(typescript.contains("name: string"))
  }

  test("handle options in a case class") {
    case class OptionCaseClass(option: Option[String]) derives TsType
    val typescript = TsEmitter.emitAll(summon[TsType[OptionCaseClass]].get)
    assertEquals(
      typescript,
      """export interface OptionCaseClass {
        |  option?: string
        |}
        |""".stripMargin
    )
  }

  test("handle all primitive types") {
    case class PrimitiveTypes(
        char: Char,
        string: String,
        byte: Byte,
        short: Short,
        int: Int,
        long: Long,
        float: Float,
        double: Double,
        boolean: Boolean,
        stringSeq: Seq[String]
    ) derives TsType
    val typescript = TsEmitter.emitAll(summon[TsType[PrimitiveTypes]].get)
    assertEquals(
      typescript,
      """export interface PrimitiveTypes {
        |  char: number
        |  string: string
        |  byte: number
        |  short: number
        |  int: number
        |  long: number
        |  float: number
        |  double: number
        |  boolean: boolean
        |  stringSeq: string[]
        |}
        |""".stripMargin
    )
  }

  test("serialize an indexed interface (Map)") {
    case class Something(values: Map[String, String]) derives TsType
    val typescript = TsEmitter.emitAll(summon[TsType[Something]].get)
    assertEquals(
      typescript,
      """export interface Something {
        |  values: { [ key: string ]: string }
        |}
        |""".stripMargin
    )
  }

  test("handle a type alias with nested types") {
    val a = TsAlias("A", TsNumber)
    val b = TsAlias("B", TsString)
    val aOrB = TsAlias("AOrB", TsUnion.of(a, b))
    val typescript = TsEmitter.emitAll(aOrB).trim
    assert(typescript.contains("type A = number"))
    assert(typescript.contains("type B = string"))
    assert(typescript.contains("type AOrB = (A | B)"))
  }

  test("handle string literal types") {
    sealed trait Geometry
    case class Point(lat: Double, lon: Double) extends Geometry
    case class Polygon(coords: Seq[Point]) extends Geometry

    val coordsType = TsTuple.of(TsNumber, TsNumber)
    val pointType: TsType[Point] =
      TsType(TsInterface("Point", ListMap("type" -> TsLiteralString("Point"), "coords" -> coordsType)))
    val polygonType: TsType[Polygon] =
      TsType(TsInterface("Polygon", ListMap("type" -> TsLiteralString("Polygon"), "coords" -> TsArray(coordsType))))
    val geometryType: TsType[Geometry] =
      TsType(TsAlias("Geometry", pointType | polygonType))

    val expectedPoint =
      """export interface Point {
        |  type: "Point"
        |  coords: [number, number]
        |}""".stripMargin
    val expectedPolygon =
      """export interface Polygon {
        |  type: "Polygon"
        |  coords: [number, number][]
        |}""".stripMargin

    val pointOutput = TsEmitter.emitAll(pointType.get).trim
    assertEquals(pointOutput, expectedPoint)

    val polygonOutput = TsEmitter.emitAll(polygonType.get).trim
    assertEquals(polygonOutput, expectedPolygon)

    val typescript = TsEmitter.emitAll(geometryType.get).trim
    assert(typescript.contains(expectedPoint))
    assert(typescript.contains(expectedPolygon))
    assert(typescript.contains("type Geometry = (Point | Polygon)"))
  }

  test("handle number literals") {
    val fourtyTwo = TsAlias("FourtyTwo", TsLiteralNumber(42))
    val typescript = TsEmitter.emitAll(fourtyTwo).trim
    assertEquals(typescript, "export type FourtyTwo = 42")
  }

  test("handle boolean literals") {
    val myBool = TsAlias("MyBool", TsLiteralBoolean(true) | TsLiteralBoolean(false))
    val typescript = TsEmitter.emitAll(myBool).trim
    assertEquals(typescript, "export type MyBool = (true | false)")
  }

  test("handle object type") {
    val x = TsAlias("X", TsObject)
    val typescript = TsEmitter.emitAll(x).trim
    assertEquals(typescript, "export type X = object")
  }

  test("serialize tagged unions correctly") {
    given StyleOptions = StyleOptions(taggedUnionDiscriminator = Some("kind"))
    val taggedUnion = TsAlias(
      "Un",
      TsUnion.of(
        TsTypeReference("A", Some(TsInterface("A", ListMap("s" -> TsString))), Some("A")),
        TsTypeReference("B", Some(TsInterface("B", ListMap("s" -> TsString))), Some("B"))
      )
    )
    val output = TsEmitter.emitAll(taggedUnion).trim
    assert(output.contains("""s: string"""))
    assert(output.contains("""kind: "A""""))
    assert(output.contains("""kind: "B""""))
    assert(output.contains("export interface A"))
    assert(output.contains("export interface B"))
    assert(output.contains("type Un = (A | B)"))
  }

  test("serialize function members of interfaces") {
    val iface = TsInterface(
      "TestInterface",
      ListMap(
        "fun1" -> TsFunction(ListMap("iarg" -> TsNumber, "sarg" -> TsString), TsVoid),
        "fun2" -> TsFunction(ListMap("farg" -> TsFunction(ListMap("iarg" -> TsNumber, "sarg" -> TsString), TsVoid)), TsNumber)
      )
    )
    val serialized = TsEmitter.emitAll(iface).trim
    assertEquals(
      serialized,
      """export interface TestInterface {
        |  fun1: (iarg: number, sarg: string) => void
        |  fun2: (farg: (iarg: number, sarg: string) => void) => number
        |}""".stripMargin
    )
  }

  test("serialize enums") {
    val tsEnum = TsEnum.of("MyEnum", "A", "B", "C")
    val serialized = TsEmitter.emitAll(tsEnum).trim
    assertEquals(
      serialized,
      """export enum MyEnum {
        |  A,
        |  B,
        |  C,
        |}""".stripMargin
    )
  }

  test("serialize const enums") {
    val tsEnum = TsEnum.of("MyEnum", "A", "B", "C").copy(const = true)
    val serialized = TsEmitter.emitAll(tsEnum).trim
    assertEquals(
      serialized,
      """export const enum MyEnum {
        |  A,
        |  B,
        |  C,
        |}""".stripMargin
    )
  }

  test("serialize numeric enums") {
    val tsEnum = TsEnum.numeric("MyEnum", "A" -> 2, "B" -> 3, "C" -> 5)
    val serialized = TsEmitter.emitAll(tsEnum).trim
    assertEquals(
      serialized,
      """export enum MyEnum {
        |  A = 2,
        |  B = 3,
        |  C = 5,
        |}""".stripMargin
    )
  }

  test("serialize string enums") {
    val tsEnum = TsEnum.string("MyEnum", "A" -> "a", "B" -> "bValue", "C" -> "c")
    val serialized = TsEmitter.emitAll(tsEnum).trim
    assertEquals(
      serialized,
      """export enum MyEnum {
        |  A = "a",
        |  B = "bValue",
        |  C = "c",
        |}""".stripMargin
    )
  }

  test("serialize a named indexed interface") {
    val iface = TsInterfaceIndexed("Something", indexName = "as", indexType = TsString, valueType = TsString)
    assertEquals(
      TsEmitter.emitAll(iface),
      """export interface Something {
        |  [ as: string ]: string
        |}
        |""".stripMargin
    )
  }

  test("serialize named function references") {
    val function = TsFunctionNamed("myfunc", TsFunction())
    val iface = TsInterface("TestInterface", ListMap("func" -> function))
    val serialized = TsEmitter.emitAll(iface).trim
    assert(serialized.contains("func: typeof myfunc"), s"Expected 'typeof myfunc' in:\n$serialized")
    assert(serialized.contains("export function myfunc(): void"), s"Expected 'export function myfunc' in:\n$serialized")
  }

  test("derive a tagged union from a Scala 3 enum") {
    enum Status derives TsType {
      case Active(since: String)
      case Inactive
      case Banned(reason: String)
    }
    val typescript = TsEmitter.emitAll(summon[TsType[Status]].get)
    assert(typescript.contains("export interface Active"), s"Expected 'export interface Active' in:\n$typescript")
    assert(typescript.contains("since: string"), s"Expected 'since: string' in:\n$typescript")
    assert(typescript.contains("""type: "Active""""), s"""Expected 'type: "Active"' in:\n$typescript""")
    assert(typescript.contains("export interface Inactive"), s"Expected 'export interface Inactive' in:\n$typescript")
    assert(typescript.contains("""type: "Inactive""""), s"""Expected 'type: "Inactive"' in:\n$typescript""")
    assert(typescript.contains("export interface Banned"), s"Expected 'export interface Banned' in:\n$typescript")
    assert(typescript.contains("""type: "Banned""""), s"""Expected 'type: "Banned"' in:\n$typescript""")
    assert(
      typescript.contains("type Status = (Active | Inactive | Banned)"),
      s"Expected 'type Status = (Active | Inactive | Banned)' in:\n$typescript"
    )
  }

  test("derive a string literal union from a Scala 3 enum with only case objects") {
    enum Color derives TsType {
      case Red, Green, Blue
    }
    val typescript = TsEmitter.emitAll(summon[TsType[Color]].get).trim
    assertEquals(
      typescript,
      """export type Color = ("Red" | "Green" | "Blue")""".stripMargin
    )
  }

  test("derive enum without discriminator field when configured") {
    given StyleOptions = StyleOptions(taggedUnionDiscriminator = None)
    enum Shape derives TsType {
      case Circle(radius: Double)
      case Square(side: Double)
    }
    val typescript = TsEmitter.emitAll(summon[TsType[Shape]].get)
    assert(typescript.contains("export interface Circle"), s"Expected 'export interface Circle' in:\n$typescript")
    assert(typescript.contains("radius: number"), s"Expected 'radius: number' in:\n$typescript")
    assert(!typescript.contains("""type: "Circle""""), s"""Did not expect 'type: "Circle"' in:\n$typescript""")
    assert(typescript.contains("type Shape = (Circle | Square)"), s"Expected 'type Shape = (Circle | Square)' in:\n$typescript")
  }

  test("derive a string literal union from a Scala 3 enum with constructor params") {
    val typescript = TsEmitter.emitAll(summon[TsType[Rating]].get).trim
    assertEquals(
      typescript,
      """export type Rating = ("Again" | "Hard" | "Good" | "Easy")""".stripMargin
    )
  }

  test("handle recursive types with a workaround") {
    case class A(b: B)
    case class B(a: A)

    given TsType[B] = {
      given TsType[A] = TsType.external("A")
      TsType.derive[B]
    }
    val aType: TsType[A] = TsType.derive[A]
    val output = TsEmitter.emitAll(aType.get)
    assert(output.contains("export interface A"), s"Expected 'export interface A' in:\n$output")
    assert(output.contains("b: B"), s"Expected 'b: B' in:\n$output")
    assert(output.contains("export interface B"), s"Expected 'export interface B' in:\n$output")
    assert(output.contains("a: A"), s"Expected 'a: A' in:\n$output")
  }

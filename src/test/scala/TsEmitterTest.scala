import scala.collection.immutable.ListMap
import TsExpr.*

class TsEmitterTest extends munit.FunSuite:
  given StyleOptions = StyleOptions()

  // --- emit: primitives ---

  test("emit primitives") {
    assertEquals(TsEmitter.emit(TsAny), "any")
    assertEquals(TsEmitter.emit(TsBoolean), "boolean")
    assertEquals(TsEmitter.emit(TsNever), "never")
    assertEquals(TsEmitter.emit(TsNull), "null")
    assertEquals(TsEmitter.emit(TsNumber), "number")
    assertEquals(TsEmitter.emit(TsObject), "object")
    assertEquals(TsEmitter.emit(TsString), "string")
    assertEquals(TsEmitter.emit(TsUndefined), "undefined")
    assertEquals(TsEmitter.emit(TsUnknown), "unknown")
    assertEquals(TsEmitter.emit(TsVoid), "void")
  }

  // --- emit: literals ---

  test("emit literal string") {
    assertEquals(TsEmitter.emit(TsLiteralString("hello")), """"hello"""")
  }

  test("emit literal number") {
    assertEquals(TsEmitter.emit(TsLiteralNumber(BigDecimal(42))), "42")
    assertEquals(TsEmitter.emit(TsLiteralNumber(BigDecimal("3.14"))), "3.14")
  }

  test("emit literal boolean") {
    assertEquals(TsEmitter.emit(TsLiteralBoolean(true)), "true")
    assertEquals(TsEmitter.emit(TsLiteralBoolean(false)), "false")
  }

  // --- emit: composite types ---

  test("emit array") {
    assertEquals(TsEmitter.emit(TsArray(TsString)), "string[]")
    assertEquals(TsEmitter.emit(TsArray(TsArray(TsNumber))), "number[][]")
  }

  test("emit tuple") {
    assertEquals(TsEmitter.emit(TsTuple.of(TsString, TsNumber)), "[string, number]")
    assertEquals(TsEmitter.emit(TsTuple(Seq.empty)), "[]")
  }

  test("emit union") {
    assertEquals(TsEmitter.emit(TsUnion(Seq.empty)), "never")
    assertEquals(TsEmitter.emit(TsUnion(Seq(TsString))), "string")
    assertEquals(TsEmitter.emit(TsUnion(Seq(TsString, TsNumber))), "(string | number)")
  }

  test("emit intersection") {
    assertEquals(
      TsEmitter.emit(TsIntersection(Seq(TsString, TsNumber))),
      "(string & number)"
    )
  }

  test("emit indexed interface") {
    assertEquals(
      TsEmitter.emit(TsIndexedInterface("key", TsString, TsNumber)),
      "{ [ key: string ]: number }"
    )
  }

  test("emit indexed interface with semicolons") {
    given StyleOptions = StyleOptions(semicolons = true)
    assertEquals(
      TsEmitter.emit(TsIndexedInterface("key", TsString, TsNumber)),
      "{ [ key: string ]: number; }"
    )
  }

  test("emit function") {
    assertEquals(
      TsEmitter.emit(TsFunction(ListMap("x" -> TsNumber, "y" -> TsNumber), TsBoolean)),
      "(x: number, y: number) => boolean"
    )
    assertEquals(
      TsEmitter.emit(TsFunction()),
      "() => void"
    )
  }

  // --- emit: named types emit as their name ---

  test("emit type reference") {
    assertEquals(TsEmitter.emit(TsTypeReference("com.example.Foo")), "Foo")
  }

  test("emit interface by name") {
    assertEquals(
      TsEmitter.emit(TsInterface("com.example.Bar", ListMap("x" -> TsNumber))),
      "Bar"
    )
  }

  test("emit alias by name") {
    assertEquals(TsEmitter.emit(TsAlias("my.MyType", TsString)), "MyType")
  }

  test("emit enum by name") {
    assertEquals(TsEmitter.emit(TsEnum.of("pkg.Color", "Red", "Green")), "Color")
  }

  // --- emitNamed ---

  test("emitNamed alias") {
    val alias = TsAlias("pkg.MyAlias", TsString)
    assertEquals(TsEmitter.emitNamed(alias), Some("export type MyAlias = string"))
  }

  test("emitNamed alias with semicolons") {
    given StyleOptions = StyleOptions(semicolons = true)
    val alias = TsAlias("pkg.MyAlias", TsString)
    assertEquals(TsEmitter.emitNamed(alias), Some("export type MyAlias = string;"))
  }

  test("emitNamed interface") {
    val iface = TsInterface("pkg.Person", ListMap("name" -> TsString, "age" -> TsNumber))
    val expected =
      """export interface Person {
        |  name: string
        |  age: number
        |}""".stripMargin
    assertEquals(TsEmitter.emitNamed(iface), Some(expected))
  }

  test("emitNamed interface with semicolons") {
    given StyleOptions = StyleOptions(semicolons = true)
    val iface = TsInterface("pkg.Person", ListMap("name" -> TsString, "age" -> TsNumber))
    val expected =
      """export interface Person {
        |  name: string;
        |  age: number;
        |}""".stripMargin
    assertEquals(TsEmitter.emitNamed(iface), Some(expected))
  }

  test("emitNamed interface with optional fields") {
    val iface = TsInterface(
      "pkg.Opt",
      ListMap("required" -> TsString, "optional" -> TsUnion(Seq(TsNumber, TsUndefined)))
    )
    val expected =
      """export interface Opt {
        |  required: string
        |  optional?: number
        |}""".stripMargin
    assertEquals(TsEmitter.emitNamed(iface), Some(expected))
  }

  test("emitNamed enum") {
    val e = TsEnum.of("pkg.Color", "Red", "Green", "Blue")
    val expected =
      """export enum Color {
        |  Red,
        |  Green,
        |  Blue,
        |}""".stripMargin
    assertEquals(TsEmitter.emitNamed(e), Some(expected))
  }

  test("emitNamed const enum with string values") {
    val e = TsEnum("pkg.Direction", const = true, ListMap(
      "Up" -> Some(TsLiteralString("UP")),
      "Down" -> Some(TsLiteralString("DOWN"))
    ))
    val expected =
      """export const enum Direction {
        |  Up = "UP",
        |  Down = "DOWN",
        |}""".stripMargin
    assertEquals(TsEmitter.emitNamed(e), Some(expected))
  }

  test("emitNamed returns None for type reference") {
    assertEquals(TsEmitter.emitNamed(TsTypeReference("pkg.Foo")), None)
  }

  test("emitNamed returns None for primitives") {
    assertEquals(TsEmitter.emitNamed(TsString), None)
  }

  // --- discoverNamed ---

  test("discoverNamed finds interface") {
    val iface = TsInterface("pkg.Foo", ListMap("x" -> TsNumber))
    assertEquals(TsEmitter.discoverNamed(iface), Set(iface))
  }

  test("discoverNamed finds nested types") {
    val inner = TsInterface("pkg.Inner", ListMap("x" -> TsNumber))
    val ref = TsTypeReference("pkg.Inner", Some(inner))
    val outer = TsInterface("pkg.Outer", ListMap("inner" -> ref))
    val discovered = TsEmitter.discoverNamed(outer)
    assert(discovered.contains(outer))
    assert(discovered.contains(ref))
  }

  test("discoverNamed through array") {
    val iface = TsInterface("pkg.Item", ListMap("v" -> TsNumber))
    val ref = TsTypeReference("pkg.Item", Some(iface))
    val arr = TsArray(ref)
    val discovered = TsEmitter.discoverNamed(arr)
    assert(discovered.contains(ref))
  }

  test("discoverNamed returns empty for primitives") {
    assertEquals(TsEmitter.discoverNamed(TsString), Set.empty[TsExpr])
  }

  test("discoverNamed injects discriminator into union members") {
    val catIface = TsInterface("pkg.Cat", ListMap("meow" -> TsBoolean))
    val dogIface = TsInterface("pkg.Dog", ListMap("bark" -> TsBoolean))
    val union = TsUnion(Seq(
      TsTypeReference("pkg.Cat", Some(catIface), Some("Cat")),
      TsTypeReference("pkg.Dog", Some(dogIface), Some("Dog"))
    ))
    val discovered = TsEmitter.discoverNamed(union)
    val catRef = discovered.collect { case r: TsTypeReference if r.qualifiedName == "pkg.Cat" => r }.head
    val catImpl = catRef.impl.get.asInstanceOf[TsInterface]
    assert(catImpl.members.contains("type"))
    assertEquals(catImpl.members("type"), TsLiteralString("Cat"))
  }

  test("discoverNamed skips discriminator when taggedUnionDiscriminator is None") {
    given StyleOptions = StyleOptions(taggedUnionDiscriminator = None)
    val catIface = TsInterface("pkg.Cat", ListMap("meow" -> TsBoolean))
    val union = TsUnion(Seq(
      TsTypeReference("pkg.Cat", Some(catIface), Some("Cat"))
    ))
    val discovered = TsEmitter.discoverNamed(union)
    val catRef = discovered.collect { case r: TsTypeReference if r.qualifiedName == "pkg.Cat" => r }.head
    val catImpl = catRef.impl.get.asInstanceOf[TsInterface]
    assert(!catImpl.members.contains("type"))
  }

  // --- emitAll ---

  test("emitAll emits complete type definitions") {
    val inner = TsInterface("pkg.Address", ListMap("street" -> TsString))
    val ref = TsTypeReference("pkg.Address", Some(inner))
    val outer = TsInterface("pkg.Person", ListMap("name" -> TsString, "address" -> ref))
    val result = TsEmitter.emitAll(outer)
    assert(result.contains("export interface Address"))
    assert(result.contains("export interface Person"))
  }

  test("emitAll emits tagged union with discriminator") {
    val catIface = TsInterface("pkg.Cat", ListMap("meow" -> TsBoolean))
    val dogIface = TsInterface("pkg.Dog", ListMap("bark" -> TsBoolean))
    val union = TsUnion(Seq(
      TsTypeReference("pkg.Cat", Some(catIface), Some("Cat")),
      TsTypeReference("pkg.Dog", Some(dogIface), Some("Dog"))
    ))
    val alias = TsAlias("pkg.Animal", union)
    val result = TsEmitter.emitAll(alias)
    assert(result.contains("export type Animal"))
    assert(result.contains("""type: "Cat""""))
    assert(result.contains("""type: "Dog""""))
  }

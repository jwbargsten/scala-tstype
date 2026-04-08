package org.bargsten.tstype

import scala.collection.immutable.ListMap
import TsExpr.*

class TsTypeDeriveTest extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "s")

  test("derive a simple case class") {
    case class Person(name: String, age: Int) derives TsType
    val ts = summon[TsType[Person]].get
    assert(ts.isInstanceOf[TsInterface])
    val iface = ts.asInstanceOf[TsInterface]
    assertEquals(iface.members("name"), TsString)
    assertEquals(iface.members("age"), TsNumber)
  }

  test("derive handles optional types") {
    case class TestOptional(opt: Option[Long]) derives TsType
    val ts = summon[TsType[TestOptional]].get
    val iface = ts.asInstanceOf[TsInterface]
    assertEquals(iface.members("opt"), TsUnion.of(TsNumber, TsUndefined))
  }

  test("derive handles nested case classes") {
    case class Inner(foo: Boolean) derives TsType
    case class Outer(inner: Inner) derives TsType
    val ts = summon[TsType[Outer]].get
    val iface = ts.asInstanceOf[TsInterface]
    assert(iface.members("inner").isInstanceOf[TsInterface])
  }

  test("derive handles multiple layers of nesting") {
    case class Nest2(prop5: String) derives TsType
    case class Nest1(prop3: String, prop4: Nest2) derives TsType
    case class TopLevel(prop1: String, prop2: Nest1) derives TsType

    val output = TsEmitter.emitAll(summon[TsType[TopLevel]].get)
    assert(output.contains("export interface TopLevel"), s"in:\n$output")
    assert(output.contains("prop1: string"), s"in:\n$output")
    assert(output.contains("prop2: Nest1"), s"in:\n$output")
    assert(output.contains("export interface Nest1"), s"in:\n$output")
    assert(output.contains("prop3: string"), s"in:\n$output")
    assert(output.contains("prop4: Nest2"), s"in:\n$output")
    assert(output.contains("export interface Nest2"), s"in:\n$output")
    assert(output.contains("prop5: string"), s"in:\n$output")
  }

  test("derive handles nested polymorphic members") {
    case class Element(foo: String) derives TsType
    case class Root(
        twoLevels: Seq[Seq[Element]],
        threeLevels: Seq[Seq[Seq[Element]]]
    ) derives TsType

    val ts = summon[TsType[Root]].get
    val iface = ts.asInstanceOf[TsInterface]
    val elemIface = summon[TsType[Element]].get

    assertEquals(iface.members("twoLevels"), TsArray(TsArray(elemIface)))
    assertEquals(iface.members("threeLevels"), TsArray(TsArray(TsArray(elemIface))))
  }

  test("derive handles polymorphic members with generated parameter types") {
    case class Element(foo: String) derives TsType
    case class Root(
        listField: Seq[Element],
        eitherField: Either[String, Element],
        tuple3Field: (Element, String, Int)
    ) derives TsType

    val ts = summon[TsType[Root]].get
    val iface = ts.asInstanceOf[TsInterface]
    val elemIface = summon[TsType[Element]].get

    assertEquals(iface.members("listField"), TsArray(elemIface))
    assertEquals(iface.members("eitherField"), TsUnion.of(TsString, elemIface))
    assertEquals(iface.members("tuple3Field"), TsTuple.of(elemIface, TsString, TsNumber))
  }

  test("derive handles sealed traits as union aliases") {
    sealed trait FooOrBar
    case class Foo(foo: String) extends FooOrBar
    case class Bar(bar: Int) extends FooOrBar

    val ts = TsType.derive[FooOrBar]
    val alias = ts.get.asInstanceOf[TsAlias]
    val output = TsEmitter.emitAll(ts.get)
    assert(output.contains("export interface Foo"), s"in:\n$output")
    assert(output.contains("foo: string"), s"in:\n$output")
    assert(output.contains("export interface Bar"), s"in:\n$output")
    assert(output.contains("bar: number"), s"in:\n$output")
    assert(output.contains("type FooOrBar = (Foo | Bar)"), s"in:\n$output")
  }

  test("derive handles sealed trait with single subclass") {
    sealed trait Single
    case class OnlyChild(foo: Int) extends Single

    val ts = TsType.derive[Single]
    val output = TsEmitter.emitAll(ts.get)
    assert(output.contains("export interface OnlyChild"), s"in:\n$output")
    assert(output.contains("foo: number"), s"in:\n$output")
  }

  test("derive can deal with seqs of nested case classes") {
    case class C(baz: String)
    case class B(bar: Seq[C])
    case class A(foo: B)
    val derived = TsType.derive[A]
    val a = derived.get.asInstanceOf[TsInterface]
    val b = a.members("foo").asInstanceOf[TsInterface]
    val seqC = b.members("bar").asInstanceOf[TsArray]
    val c = seqC.elementType.asInstanceOf[TsInterface]
    assertEquals(c.members("baz"), TsString)
  }

  test("derive always produces a fresh derivation") {
    case class A(foo: String)
    given tsA: TsType[A] = TsType(TsNumber)
    val derived = TsType.derive[A]
    // derive ignores the existing given and re-derives from the case class
    assertEquals(derived.get.asInstanceOf[TsInterface].members("foo"), TsString)
  }

  test("derive with custom polymorphic types in scope") {
    class CustomTsType
    given TsType[CustomTsType] = TsType(TsString)
    case class ContainsGeneric(foos: Seq[CustomTsType]) derives TsType

    val ts = summon[TsType[ContainsGeneric]].get
    val iface = ts.asInstanceOf[TsInterface]
    assertEquals(iface.members("foos"), TsArray(TsString))
  }
  
  test("derive case class with literal type fields") {
    case class Literal(
        a: "Hello!",
        b: 42,
        c: true
    ) derives TsType

    val output = TsEmitter.emitAll(summon[TsType[Literal]].get).trim
    assertEquals(
      output,
      """export interface Literal {
        |  a: "Hello!"
        |  b: 42
        |  c: true
        |}""".stripMargin
    )
  }

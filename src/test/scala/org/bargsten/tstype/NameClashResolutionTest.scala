package org.bargsten.tstype

import scala.collection.immutable.ListMap
import TsExpr.*

class NameClashResolutionTest extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "s")

  test("no rename when no clashes") {
    val types: Set[TsExpr] = Set(
      TsInterface("com.example.Foo", ListMap("x" -> TsString)),
      TsInterface("com.example.Bar", ListMap("y" -> TsNumber))
    )
    val resolved = TsEmitter.resolveNameClashes(types)
    assertEquals(resolved.map(_.name), Set("Foo", "Bar"))
  }

  test("disambiguate two types with the same name using package segments") {
    val types: Set[TsExpr] = Set(
      TsInterface("org.bargsten.fsrs.Card", ListMap("x" -> TsString)),
      TsInterface("org.bargsten.learn.Card", ListMap("y" -> TsNumber))
    )
    val resolved = TsEmitter.resolveNameClashes(types)
    assertEquals(resolved.map(_.name), Set("FsrsCard", "LearnCard"))
  }

  test("use deeper package segments when one level is not enough") {
    val types: Set[TsExpr] = Set(
      TsInterface("org.bargsten.fsrs.abc.Card", ListMap("x" -> TsString)),
      TsInterface("org.bargsten.learn.abc.Card", ListMap("y" -> TsNumber))
    )
    val resolved = TsEmitter.resolveNameClashes(types)
    assertEquals(resolved.map(_.name), Set("FsrsAbcCard", "LearnAbcCard"))
  }

  test("leave types without package segments unchanged") {
    val types: Set[TsExpr] = Set(
      TsInterface("Foo", ListMap("x" -> TsString)),
      TsInterface("Bar", ListMap("y" -> TsNumber))
    )
    val resolved = TsEmitter.resolveNameClashes(types)
    assertEquals(resolved.map(_.name), Set("Foo", "Bar"))
  }

  test("update references when renaming types") {
    val fsrsCard = TsInterface("org.bargsten.fsrs.Card", ListMap("id" -> TsString))
    val learnCard = TsInterface(
      "org.bargsten.learn.Card",
      ListMap("state" -> TsTypeReference("org.bargsten.fsrs.Card", Some(fsrsCard)))
    )
    val types: Set[TsExpr] = Set(fsrsCard, learnCard)
    val resolved = TsEmitter.resolveNameClashes(types)

    val resolvedLearn = resolved.collectFirst { case i: TsInterface if i.name == "LearnCard" => i }.get
    val stateType = resolvedLearn.members("state")
    assert(stateType.isInstanceOf[TsTypeReference])
    assertEquals(stateType.name, "FsrsCard")
  }

  test("handle TsAlias name clashes") {
    val types: Set[TsExpr] = Set(
      TsAlias("com.app.model.Status", TsString),
      TsAlias("com.app.other.Status", TsNumber)
    )
    val resolved = TsEmitter.resolveNameClashes(types)
    assertEquals(resolved.map(_.name), Set("ModelStatus", "OtherStatus"))
  }

  test("emitAll produces correct output with disambiguated names") {
    given StyleOptions = StyleOptions()
    val fsrsCard = TsInterface("org.bargsten.fsrs.Card", ListMap("id" -> TsString))
    val learnCard = TsInterface("org.bargsten.learn.Card", ListMap("name" -> TsString))
    val result = TsEmitter.emitAll(Seq(fsrsCard, learnCard))
    assert(result.contains("interface FsrsCard"), s"Expected 'interface FsrsCard' in:\n$result")
    assert(result.contains("interface LearnCard"), s"Expected 'interface LearnCard' in:\n$result")
    assert(!result.contains("interface Card"), s"Did not expect 'interface Card' in:\n$result")
  }

  test("emitAll leaves non-clashing types unchanged") {
    given StyleOptions = StyleOptions()
    val foo = TsInterface("com.example.Foo", ListMap("x" -> TsString))
    val bar = TsInterface("com.example.Bar", ListMap("y" -> TsNumber))
    val result = TsEmitter.emitAll(Seq(foo, bar))
    assert(result.contains("interface Foo"), s"Expected 'interface Foo' in:\n$result")
    assert(result.contains("interface Bar"), s"Expected 'interface Bar' in:\n$result")
  }

  test("disambiguate strips '$' from module package segments") {
    val types: Set[TsExpr] = Set(
      TsInterface("org.bargsten.gravel.chat.model$.Message", ListMap("x" -> TsString)),
      TsInterface("org.bargsten.gravel.chat.api.dto.Message", ListMap("y" -> TsNumber))
    )
    val resolved = TsEmitter.resolveNameClashes(types)
    val names = resolved.map(_.name)
    assertEquals(names, Set("ModelMessage", "DtoMessage"))
    assert(!names.exists(_.contains("$")), s"No name should contain '$$', got: $names")
  }

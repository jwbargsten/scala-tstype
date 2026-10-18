package org.bargsten.apitest

import org.bargsten.tstype.*

class PublicApiTest extends munit.FunSuite {

  test("emitter and macro internals are not public") {
    assertNotEquals(compileErrors("TsEmitter.resolveNameClashes(Set.empty)"), "")
    assertNotEquals(compileErrors("TsEmitter.discoverNamed(TsExpr.TsString)"), "")
    assertNotEquals(compileErrors("TsEmitter.emitNamed(TsExpr.TsString)(using StyleOptions())"), "")
    assertNotEquals(compileErrors("TsTypeMacros"), "")
    assertNotEquals(compileErrors("TsType.getT[Int]"), "")
    assertEquals(compileErrors("TsEmitter.emit(TsExpr.TsString)"), "")
    assertEquals(compileErrors("TsEmitter.emitAll(Seq(TsExpr.TsString))(using StyleOptions())"), "")
  }
}

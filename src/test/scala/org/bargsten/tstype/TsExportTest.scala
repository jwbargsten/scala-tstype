package org.bargsten.tstype

import java.nio.file.Files

case class Bar(name: String) derives TsType
case class Foo(name: String, age: Int, bars: Seq[Bar]) derives TsType

class TsExportTest extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "s")

  test("toTypescript renders case classes correctly") {
    val result = TsExport.toTypescript(Seq(TsType.derive[Foo]))
    assert(result.contains("export interface Foo"), s"Expected 'export interface Foo' in:\n$result")
    assert(result.contains("name: string"), s"Expected 'name: string' in:\n$result")
    assert(result.contains("age: number"), s"Expected 'age: number' in:\n$result")
    assert(result.contains("bars: Bar[]"), s"Expected 'bars: Bar[]' in:\n$result")
    assert(result.contains("export interface Bar"), s"Expected 'export interface Bar' in:\n$result")
  }

  test("toTypescript respects StyleOptions") {
    val result = TsExport.toTypescript(
      Seq(TsType.derive[Bar]),
      styleOptions = StyleOptions(semicolons = true)
    )
    assert(result.contains("name: string;"), s"Expected 'name: string;' in:\n$result")
  }

  test("toTypescriptFile writes to a file with header") {
    val tmpFile = Files.createTempFile("tsexport-test", ".ts")
    try
      TsExport.toTypescriptFile(Seq(TsType.derive[Foo]), tmpFile.toString)
      val content = Files.readString(tmpFile)
      assert(
        content.startsWith("// DO NOT EDIT: generated file by scala-tstype"),
        s"Expected header in:\n$content"
      )
      assert(content.contains("export interface Foo"), s"Expected 'export interface Foo' in:\n$content")
      assert(content.contains("export interface Bar"), s"Expected 'export interface Bar' in:\n$content")
    finally Files.deleteIfExists(tmpFile)
  }

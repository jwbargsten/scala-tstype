package org.bargsten.tstype

import java.nio.file.{Files, Path}

object TsExport:
  def toTypescript(
      types: Seq[TsType[?]],
      styleOptions: StyleOptions = StyleOptions()
  ): String =
    given StyleOptions = styleOptions
    TsEmitter.emitAll(types.map(_.get))

  def toTypescriptFile(
      types: Seq[TsType[?]],
      targetFile: String,
      styleOptions: StyleOptions = StyleOptions(),
      header: String = "// DO NOT EDIT: generated file by scala-tstype"
  ): Unit =
    val content = toTypescript(types, styleOptions)
    val fullContent = if header.nonEmpty then s"$header\n\n$content" else content
    val path = Path.of(targetFile)
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.writeString(path, fullContent)

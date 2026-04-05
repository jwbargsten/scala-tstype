package org.bargsten.tstype

case class StyleOptions(
    semicolons: Boolean = false,
    taggedUnionDiscriminator: Option[String] = Some("type")
):
  val sc: String = if semicolons then ";" else ""

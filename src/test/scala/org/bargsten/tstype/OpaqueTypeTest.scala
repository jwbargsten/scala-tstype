package org.bargsten.tstype

import org.bargsten.tstype.OpaqueTypes.{Player, Score, UserId}
import org.bargsten.tstype.TsExpr.*
import org.bargsten.tstype.TsType

import scala.collection.immutable.ListMap

object OpaqueTypes:
  opaque type UserId = String
  object UserId:
    def apply(s: String): UserId = s
    given TsType[UserId] = TsType(TsString)

  opaque type Score = Int
  object Score:
    def apply(i: Int): Score = i
    given TsType[Score] = TsType(TsNumber)

  case class Player(name: String, score: Score, id: UserId) derives TsType

class OpaqueTypeTest extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "s")
  import OpaqueTypes.*

  test("opaque type with explicit given import") {
    import OpaqueTypes.Score.given
    assertEquals(summon[TsType[Score]].get, TsNumber)
  }

  test("opaque type found from companion without explicit import") {
    assertEquals(summon[TsType[UserId]].get, TsString)
  }

  test("derive case class with opaque type fields") {
    val derived = summon[TsType[Player]].get
    assertEquals(
      derived,
      TsInterface("OpaqueTypes$.Player", ListMap("name" -> TsString, "score" -> TsNumber, "id" -> TsString))
    )
  }

  test("derive case class with opaque types from companion scope") {
    case class UserRef(id: UserId, label: String) derives TsType
    val derived = summon[TsType[UserRef]].get
    assertEquals(
      derived,
      TsInterface("OpaqueTypeTest._$UserRef", ListMap("id" -> TsString, "label" -> TsString))
    )
  }

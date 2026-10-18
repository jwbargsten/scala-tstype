package org.bargsten.tstype

import org.bargsten.tstype.TsExpr.TsInterface
import org.bargsten.tstype.syntax.*

class SyntaxTest extends munit.FunSuite {
  case class Person(
      name: String,
      email: Email,
      age: Option[Int],
      ssn: Option[Int],
      job: Job
  )

  case class Email(address: String)

  case class Job(tasks: Seq[String], boss: String)

  test("manual member removal") {
    given x: TsType[Job] = TsType.derive[Job].get match {
      case tsi: TsInterface => TsType(tsi.copy(members = tsi.members - "boss"))
      case _                => ???
    }

    val y = TsType.derive[Person].get match {
      case tsi: TsInterface => tsi.copy(members = tsi.members - "ssn")
      case _                => ???
    }

    val res = TsExport.toTypescript(Seq(TsType(y)))
    val expected =
      """|export interface Person {
         |  name: string
         |  email: Email
         |  age?: number
         |  job: Job
         |}
         |
         |export interface Email {
         |  address: string
         |}
         |
         |export interface Job {
         |  tasks: string[]
         |}
         |""".stripMargin
    assertEquals(res, expected)
  }

  test("member removal via dsl") {
    given TsType[Email] = TsType.sameAs[Email, String]
    given TsType[Job] = TsType.derive[Job] - "boss"

    val y = TsType.derive[Person] - "ssn"

    val res = TsExport.toTypescript(Seq(y))
    val expected =
      """|export interface Person {
         |  name: string
         |  email: string
         |  age?: number
         |  job: Job
         |}
         |
         |export interface Job {
         |  tasks: string[]
         |}
         |""".stripMargin
    assertEquals(res, expected)
  }
}

class SyntaxErrorTest extends munit.FunSuite {
  test("adding a member to a non-interface says so") {
    val t: TsType[String] = TsType(TsExpr.TsString)
    val e = intercept[IllegalArgumentException](t + ("x" -> TsType[Int](TsExpr.TsNumber)))
    assertEquals(e.getMessage, "you can only add fields to interfaces (case classes)")
    val e2 = intercept[IllegalArgumentException](t ++ Seq("x" -> TsType[Int](TsExpr.TsNumber)))
    assertEquals(e2.getMessage, "you can only add fields to interfaces (case classes)")
  }
}

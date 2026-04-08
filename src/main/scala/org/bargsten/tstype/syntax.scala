package org.bargsten.tstype

import org.bargsten.tstype.TsExpr.TsInterface

object syntax {
  extension [A](t: TsType[A]) {
    infix def -(field: String): TsType[A] = t.get match
      case tsi: TsInterface => TsType(tsi.copy(members = tsi.members - field))
      case _                => throw IllegalArgumentException("you can only subtract fields from interfaces (case classes)")

    infix def --(fields: Iterable[String]): TsType[A] = t.get match
      case tsi: TsInterface => TsType(tsi.copy(members = tsi.members -- fields))
      case _                => throw IllegalArgumentException("you can only subtract fields from interfaces (case classes)")

    infix def +(member: (String, TsType[?])): TsType[A] = t.get match
      case tsi: TsInterface => TsType(tsi.copy(members = tsi.members + (member._1 -> member._2.get)))
      case _                => throw IllegalArgumentException("you can only subtract fields from interfaces (case classes)")

    infix def ++(members: Iterable[(String, TsType[?])]): TsType[A] = t.get match
      case tsi: TsInterface => TsType(tsi.copy(members = tsi.members ++ members.map(m => m._1 -> m._2.get)))
      case _                => throw IllegalArgumentException("you can only subtract fields from interfaces (case classes)")
  }
}

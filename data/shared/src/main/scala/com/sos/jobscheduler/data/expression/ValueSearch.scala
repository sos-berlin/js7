package com.sos.jobscheduler.data.expression

/**
  * @author Joacim Zschimmer
  */
final case class ValueSearch(where: ValueSearch.Where, what: ValueSearch.What)

object ValueSearch
{
  sealed trait Where
  case object Argument extends Where
  case object LastOccurred extends Where
  final case class LastExecuted(search: PositionSearch) extends Where

  sealed trait What
  final case class KeyValue(key: String) extends What
  case object ReturnCode extends What
}

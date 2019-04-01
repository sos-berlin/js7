package com.sos.jobscheduler.data.expression

/**
  * @author Joacim Zschimmer
  */
final case class ValueSearch(where: ValueSearch.Where, key: String)

object ValueSearch
{
  sealed trait Where

  case object Argument extends Where

  case object LastOccurred extends Where

  final case class LastExecuted(search: PositionSearch) extends Where
}

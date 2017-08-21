package com.sos.jobscheduler.agent.command

import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class CommandHandlerOverview(
  currentCommandCount: Int,
  totalCommandCount: Long)

object CommandHandlerOverview {
  implicit val jsonFormat = jsonFormat2(apply)
}

package com.sos.jobscheduler.agent.command

import scala.collection.immutable.Seq
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class CommandHandlerDetailed(commandRuns: Seq[CommandRunOverview])

object CommandHandlerDetailed {
  implicit val jsonFormat = jsonFormat1(apply)
}

package com.sos.jobscheduler.agent.command

import com.sos.jobscheduler.agent.data.commands.Command
import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class CommandRunOverview(
  internalId: InternalCommandId,
  startedAt: Instant,
  command: Command)

object CommandRunOverview {
  implicit val MyJsonFormat = jsonFormat3(apply)
}

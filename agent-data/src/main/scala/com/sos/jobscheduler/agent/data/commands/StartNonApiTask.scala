package com.sos.scheduler.engine.agent.data.commands

import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class StartNonApiTask(meta: Option[StartTask.Meta])
extends StartTask

object StartNonApiTask {
  val SerialTypeName = "StartNonApiTask"
  implicit val MyJsonFormat = jsonFormat1(apply)
}

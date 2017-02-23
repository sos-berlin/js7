package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.agent.data.commands.StartTask.Meta
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class StartApiTask(
  meta: Option[Meta],
  javaOptions: String,
  javaClasspath: String)
extends StartTask

object StartApiTask {
  val SerialTypeName = "StartApiTask"
  implicit val MyJsonFormat = jsonFormat3(apply)
}

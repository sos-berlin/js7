package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.agent.data.commandresponses.EmptyResponse
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class MoveFile(path: String, toDirectory: String) extends FileCommand {
  type Response = EmptyResponse.type
}

object MoveFile {
  val SerialTypeName = "MoveFile"
  implicit val MyJsonFormat = jsonFormat2(apply)
}

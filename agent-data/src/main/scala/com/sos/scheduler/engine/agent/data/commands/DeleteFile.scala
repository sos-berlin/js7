package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.agent.data.commandresponses.EmptyResponse
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class DeleteFile(path: String) extends FileCommand {
  type Response = EmptyResponse.type
}

object DeleteFile {
  val SerialTypeName = "DeleteFile"
  implicit val MyJsonFormat = jsonFormat1(apply)
}

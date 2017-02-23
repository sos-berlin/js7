package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.agent.data.commandresponses.EmptyResponse
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
case object NoOperation extends Command {
  type Response = EmptyResponse.type

  val SerialTypeName = "NoOperation"
  implicit val jsonFormat = jsonFormat0(() ⇒ NoOperation)
}

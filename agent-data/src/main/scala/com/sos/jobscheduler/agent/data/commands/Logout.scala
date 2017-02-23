package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.agent.data.commandresponses.EmptyResponse
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
case object Logout extends Command {
  type Response = EmptyResponse.type

  val SerialTypeName = "Logout"
  implicit val jsonFormat = jsonFormat0(() ⇒ Logout)
}

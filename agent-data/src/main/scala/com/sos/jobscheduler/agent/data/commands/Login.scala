package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.agent.data.commandresponses.LoginResponse
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
case object Login extends Command {
  type Response = LoginResponse

  val SerialTypeName = "Login"
  implicit val jsonFormat = jsonFormat0(() ⇒ Login)
}

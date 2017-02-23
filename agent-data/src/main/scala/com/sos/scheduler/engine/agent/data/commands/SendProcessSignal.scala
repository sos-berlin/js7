package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.commandresponses.EmptyResponse
import com.sos.scheduler.engine.base.process.ProcessSignal
import spray.json.DefaultJsonProtocol._

/**
 * Sends a Unix type process signal like SIGTERM to a process.
 *
 * @author Joacim Zschimmer
 */
final case class SendProcessSignal(agentTaskId: AgentTaskId, signal: ProcessSignal)
extends TaskCommand {
  type Response = EmptyResponse.type
}

object SendProcessSignal {
  val SerialTypeName = "SendProcessSignal"
  implicit val MyJsonFormat = {
    implicit def UnixProcessSignalJsonFormat = ProcessSignal.MyJsonFormat
    jsonFormat2(apply)
  }
}

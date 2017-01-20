package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.agent.data.commandresponses.EmptyResponse
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Duration
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class Terminate(
  sigtermProcesses: Boolean = false,
  sigkillProcessesAfter: Option[Duration] = None)
extends TerminateOrAbort {
  type Response = EmptyResponse.type
}

object Terminate {
  val SerialTypeName = "Terminate"
  val MaxDuration = 31 * 24.h
  implicit val MyJsonFormat = jsonFormat2(apply)
}

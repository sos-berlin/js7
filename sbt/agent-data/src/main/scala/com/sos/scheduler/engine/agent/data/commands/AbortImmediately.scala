package com.sos.scheduler.engine.agent.data.commands

import spray.json.DefaultJsonProtocol.jsonFormat0

/**
 * @author Joacim Zschimmer
 */
case object AbortImmediately extends TerminateOrAbort {

  val SerialTypeName = "AbortImmediately"

  /** The JVM is halted before responding. */
  type Response = Nothing

  implicit val MyJsonFormat = jsonFormat0(() ⇒ AbortImmediately)
}

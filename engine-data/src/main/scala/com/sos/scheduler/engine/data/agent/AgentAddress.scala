package com.sos.scheduler.engine.data.agent

import com.sos.scheduler.engine.base.generic.IsString

/**
  * The URL of the Agent. Should not end with a slash.
  *
  * @author Joacim Zschimmer
  */
final case class AgentAddress(string: String) extends IsString

object AgentAddress extends IsString.HasJsonFormat[AgentAddress]

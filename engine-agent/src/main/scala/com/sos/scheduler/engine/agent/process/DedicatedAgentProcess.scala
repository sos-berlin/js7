package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.common.scalautil.Logger

/**
 * @author Joacim Zschimmer
 */
final class DedicatedAgentProcess(val arguments: AgentProcessArguments) extends AgentProcess {

  def id = arguments.processId

  def start() = ???

  def close() = ???

  def kill() = ???
}

object DedicatedAgentProcess {
  private val logger = Logger(getClass)
}

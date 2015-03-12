package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.data.agent.AgentProcessId

/**
 * @author Joacim Zschimmer
 */
trait AgentProcess extends AutoCloseable {

  def id: AgentProcessId

  def start(): Unit

  def kill(): Unit
}

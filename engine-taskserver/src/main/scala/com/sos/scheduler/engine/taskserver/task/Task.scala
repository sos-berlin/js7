package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.agent.data.AgentTaskId

/**
 * @author Joacim Zschimmer
 */
trait Task extends AutoCloseable {

  def start(): Boolean

  def end(): Unit

  def step(): Any

  def callIfExists(methodWithSignature: String): Any

  protected def agentTaskId: AgentTaskId

  protected def jobName: String

  override def toString = s"${getClass.getSimpleName}($agentTaskId $jobName)"
}

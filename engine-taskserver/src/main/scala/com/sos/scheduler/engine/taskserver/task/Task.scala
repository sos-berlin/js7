package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.taskserver.task.process.Processes.Pid

/**
 * @author Joacim Zschimmer
 */
private[task] trait Task extends AutoCloseable {

  def start(): Boolean

  def end(): Unit

  def step(): Any

  def callIfExists(methodWithSignature: String): Any

  protected val commonArguments: CommonArguments

  override def toString = {
    import commonArguments.{agentTaskId, jobName}
    s"${getClass.getSimpleName}($agentTaskId $jobName)"
  }

  def pidOption: Option[Pid]
}

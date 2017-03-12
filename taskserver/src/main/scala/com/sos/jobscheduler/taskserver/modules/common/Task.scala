package com.sos.jobscheduler.taskserver.modules.common

import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.common.time.ScalaTime._

/**
 * @author Joacim Zschimmer
 */
private[taskserver] trait Task extends AutoCloseable {

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

object Task {
  private[taskserver] val LogPollPeriod = 5.s
  private[taskserver] val LogBatchThreshold = 1000*1000  // Cut-off count of characters to log (and transfer) at once
}

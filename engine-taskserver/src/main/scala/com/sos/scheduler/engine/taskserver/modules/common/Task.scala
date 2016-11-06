package com.sos.scheduler.engine.taskserver.modules.common

import com.sos.scheduler.engine.common.process.Processes.Pid
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
private[taskserver] trait Task extends AutoCloseable {

  def start(): Future[Boolean]

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

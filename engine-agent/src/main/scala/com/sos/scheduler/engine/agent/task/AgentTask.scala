package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.views.TaskOverview
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.taskserver.TaskServer
import com.sos.scheduler.engine.tunnel.core.TunnelClient
import java.time.Instant
import scala.concurrent.Future

/**
* @author Joacim Zschimmer
*/
private[task] final class AgentTask(val id: AgentTaskId, tunnel: TunnelClient, val taskServer: TaskServer)
extends AutoCloseable {

  val startedAt = Instant.now()

  def close(): Unit = {
    try tunnel.close()  // Close tunnel first, then task server
    finally taskServer.close()
  }

  def start(): Unit = taskServer.start()

  def sendProcessSignal(signal: ProcessSignal): Unit = taskServer.sendProcessSignal(signal)

  def terminated: Future[Unit] = taskServer.terminated

  def overview = TaskOverview(
    id,
    tunnel.id,
    taskServer.taskStartArguments.masterAddress,  // With a tunnel, this is the local proxy address (not very useful) !!!
    startedAt)

  private[task] def tunnelToken = tunnel.tunnelToken

  override def toString = s"AgentTask($id)"
}

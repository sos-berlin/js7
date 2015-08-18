package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.views.ProcessOverview
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.taskserver.TaskServer
import com.sos.scheduler.engine.tunnel.core.TunnelClient
import java.time.Instant
import scala.concurrent.Future

/**
* @author Joacim Zschimmer
*/
private[process] final class AgentProcess(val id: AgentProcessId, tunnel: TunnelClient, val taskServer: TaskServer)
extends AutoCloseable {

  val startedAt = Instant.now()

  def close(): Unit = {
    try tunnel.close()  // Close tunnel first, then task server
    finally taskServer.close()
  }

  def start(): Unit = taskServer.start()

  def sendProcessSignal(signal: ProcessSignal): Unit = taskServer.sendProcessSignal(signal)

  def terminated: Future[Unit] = taskServer.terminated

  def overview = ProcessOverview(
    id,
    tunnel.id,
    taskServer.taskStartArguments.controllerAddress,  // With a tunnel, this is the local proxy address (not very usefull) !!!
    startedAt)

  private[process] def tunnelToken = tunnel.tunnelToken

  override def toString = s"AgentProcess($id)"
}

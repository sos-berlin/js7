package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.views.ProcessOverview
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.taskserver.TaskServer
import java.time.Instant
import scala.concurrent.Future

/**
* @author Joacim Zschimmer
*/
final class AgentProcess(val id: AgentProcessId, val taskServer: TaskServer) extends AutoCloseable {

  val startedAt = Instant.now()

  def close(): Unit = taskServer.close()

  def start(): Unit = taskServer.start()

  def sendProcessSignal(signal: ProcessSignal): Unit = taskServer.sendProcessSignal(signal)

  def terminated: Future[Unit] = taskServer.terminated

  def overview = ProcessOverview(
    id,
    taskServer.taskStartArguments.controllerAddress,
    startedAt)

  def tunnelTokenOption = taskServer.taskStartArguments.tunnelTokenOption

  override def toString = s"AgentProcess($id)"
}

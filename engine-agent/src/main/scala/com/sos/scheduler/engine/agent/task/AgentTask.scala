package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.views.TaskOverview
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.taskserver.TaskServer
import com.sos.scheduler.engine.taskserver.task.TaskArguments
import com.sos.scheduler.engine.tunnel.server.TunnelHandle
import java.time.Instant
import scala.concurrent.Future
import scala.util.Success

/**
* @author Joacim Zschimmer
*/
private[task] final class AgentTask(val id: AgentTaskId, tunnel: TunnelHandle, val taskServer: TaskServer, taskArgumentsFuture: Future[TaskArguments])
extends AutoCloseable {

  val startedAt = Instant.now()

  def close(): Unit =
    try tunnel.close()  // Close tunnel first, then task server
    finally taskServer.close()

  def start(): Unit = taskServer.start()

  def sendProcessSignal(signal: ProcessSignal): Unit = taskServer.sendProcessSignal(signal)

  def terminated: Future[Unit] = taskServer.terminated

  def overview = TaskOverview(
    id,
    tunnel.id,
    startedAt,
    startedByIp = tunnel.startedByHttpIpOption,
    arguments = taskArgumentsFuture.value collect {
      case Success(a) ⇒
        TaskOverview.Arguments(
          taskId = a.taskId,
          jobName = a.jobName,
          language = a.moduleLanguage.string,
          javaClassName = a.javaClassNameOption,
          monitorCount = a.monitors.size)
    })

  private[task] def tunnelToken = tunnel.tunnelToken

  override def toString = s"AgentTask($id)"
}

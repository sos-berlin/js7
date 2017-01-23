package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.commands.StartTask
import com.sos.scheduler.engine.agent.data.views.TaskOverview
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.base.utils.HasKey
import com.sos.scheduler.engine.common.process.Processes.Pid
import com.sos.scheduler.engine.common.scalautil.Closers._
import com.sos.scheduler.engine.taskserver.TaskServer
import com.sos.scheduler.engine.taskserver.task.TaskArguments
import com.sos.scheduler.engine.tunnel.server.TunnelHandle
import java.time.Instant
import scala.concurrent.Future
import scala.util.Success

/**
* @author Joacim Zschimmer
*/
private[task] trait AgentTask
extends AutoCloseable
with HasKey {

  final type Key = AgentTaskId
  final def key = id

  def id: AgentTaskId

  def startMeta: StartTask.Meta

  protected def taskArgumentsFuture: Future[TaskArguments]

  /**
    * ReleaseCall has been called on RemoteModuleInstanceServer.
    */
  protected def taskReleaseFuture: Future[Unit]

  protected def tunnel: TunnelHandle

  protected def taskServer: TaskServer

  private val startedAt = Instant.now()

  final def closeTunnelAndTaskServer() = closeOrdered(tunnel, taskServer)  // Close tunnel before taskServer

  final def closeTunnel() = tunnel.close()

  final def start(): Unit = taskServer.start()

  final def onTunnelInactivity(callback: Instant ⇒ Unit): Unit = tunnel.onInactivity(callback)

  final def sendProcessSignal(signal: ProcessSignal): Unit = taskServer.sendProcessSignal(signal)

  final def deleteLogFiles(): Unit = taskServer.deleteLogFiles()

  final def terminated: Future[Unit] = taskServer.terminated

  final def pidOption: Option[Pid] = taskServer.pidOption

  final def overview = TaskOverview(
    id,
    pid = taskServer.pidOption map { _.number },
    tunnel.id,
    startedAt,
    startedByHttpIp = tunnel.startedByHttpIpOption,
    startMeta = AgentTask.this.startMeta,
    arguments = taskArgumentsFuture.value collect {
      case Success(a) ⇒
        TaskOverview.Arguments(
          language = a.rawModuleArguments.language.string,
          javaClassName = a.rawModuleArguments.javaClassNameOption,
          monitorCount = a.rawMonitorArguments.size)
    })

  private[task] final def isReleasedCalled = taskReleaseFuture.isCompleted

  private[task] final def tunnelToken = tunnel.tunnelToken

  override def toString = s"$id: $taskServer"
}

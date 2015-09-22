package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.views.TaskOverview
import com.sos.scheduler.engine.agent.task.AgentTask._
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.taskserver.TaskServer
import com.sos.scheduler.engine.taskserver.task.TaskArguments
import com.sos.scheduler.engine.tunnel.server.TunnelHandle
import java.time.Instant
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

/**
* @author Joacim Zschimmer
*/
private[task] final class AgentTask(val id: AgentTaskId, tunnel: TunnelHandle, val taskServer: TaskServer, taskArgumentsFuture: Future[TaskArguments])
extends AutoCloseable {

  val startedAt = Instant.now()
  private val taskArgumentsMapPromise = Promise[Map[String, String]]()

  def close(): Unit =
    try tunnel.close()  // Close tunnel first, then task server
    finally taskServer.close()

  def start(): Unit = taskServer.start()

  def sendProcessSignal(signal: ProcessSignal): Unit = taskServer.sendProcessSignal(signal)

  def terminated: Future[Unit] = taskServer.terminated

  def overview = TaskOverview(
    id,
    tunnel.id,
    taskServer.taskStartArguments.masterAddress,  // With a tunnel, this is the local proxy address (not very useful) !!!
    startedAt,
    taskArguments = taskArgumentsMap)

  private def taskArgumentsMap: Map[String, String] = {
    if (!taskArgumentsMapPromise.isCompleted) {
      taskArgumentsFuture.value match {
        case Some(Success(a)) ⇒ taskArgumentsMapPromise.success(taskArgumentsToMap(a))
        case Some(Failure(t)) ⇒
          taskArgumentsMapPromise.failure(t)
          logger.error(s"taskArgumentsMap: $t", t)
        case _ ⇒ Map()
      }
    }
    taskArgumentsMapPromise.future.value match {
      case Some(Success(o)) ⇒ o
      case Some(Failure(t)) ⇒ Map("error" → t.getClass.getSimpleName)
      case None ⇒ Map()
    }
  }

  private[task] def tunnelToken = tunnel.tunnelToken

  override def toString = s"AgentTask($id)"
}

object AgentTask {
  private val logger = Logger(getClass)

  private def taskArgumentsToMap(a: TaskArguments) = Map[String, String](
    "language" → a.moduleLanguage.toString
  )
}

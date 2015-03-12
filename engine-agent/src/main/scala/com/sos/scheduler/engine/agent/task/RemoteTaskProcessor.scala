package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.agent.commands.{CloseRemoteTask, CloseRemoteTaskResponse, RemoteTaskCommand, Response, StartRemoteTask, StartRemoteTaskResponse}
import com.sos.scheduler.engine.agent.task.RemoteTaskProcessor._
import com.sos.scheduler.engine.common.scalautil.{Logger, ScalaConcurrentHashMap}
import com.sos.scheduler.engine.data.agent.RemoteTaskId
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class RemoteTaskProcessor @Inject private(newRemoteTaskId: () ⇒ RemoteTaskId, newRemoteTask: TaskStartArguments ⇒ RemoteTask) {

  private val taskRegister = new ScalaConcurrentHashMap[RemoteTaskId, RemoteTask] {
    override def default(id: RemoteTaskId) = throwUnknownTask(id)
  }

  def executeCommand(command: RemoteTaskCommand) = Future[Response] {
    command match {
      case StartRemoteTask(controllerAddress, usesApi, javaOptions, javaClasspath) ⇒
        val remoteTaskId = newRemoteTaskId()
        val startArguments = TaskStartArguments(
          remoteTaskId,
          controllerAddress = controllerAddress,
          usesApi = usesApi,
          javaOptions = javaOptions,
          javaClasspath = javaClasspath)
        val task = newRemoteTask(startArguments)
        assert(task.id == remoteTaskId)
        taskRegister += task.id → task
        task.start()
        StartRemoteTaskResponse(task.id)

      case CloseRemoteTask(remoteTaskId, kill) ⇒
        val task = taskRegister.remove(remoteTaskId) getOrElse throwUnknownTask(remoteTaskId)
        if (kill) tryKillTask(task)
        task.close()
        CloseRemoteTaskResponse
    }
  }
}

private object RemoteTaskProcessor {
  private val logger = Logger(getClass)

  private def tryKillTask(task: RemoteTask) =
    try task.kill()
    catch { case NonFatal(t) ⇒ logger.warn(s"Kill $task failed: $t") }

  private def throwUnknownTask(taskId: RemoteTaskId) = throw new NoSuchElementException(s"Unknown Task '$taskId'")
}

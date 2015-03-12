package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.agent.commands.{CloseProcess, CloseProcessResponse, ProcessCommand, Response, StartDedicatedProcess, StartProcess, StartProcessResponse, StartThread}
import com.sos.scheduler.engine.agent.process.ProcessCommandExecutor._
import com.sos.scheduler.engine.common.scalautil.{Logger, ScalaConcurrentHashMap}
import com.sos.scheduler.engine.data.agent.AgentProcessId
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class ProcessCommandExecutor @Inject private(newAgentProcessId: () ⇒ AgentProcessId, newAgentProcess: AgentProcessArguments ⇒ AgentProcess) {

  private val idToProcess = new ScalaConcurrentHashMap[AgentProcessId, AgentProcess] {
    override def default(id: AgentProcessId) = throwUnknownProcess(id)
  }

  def executeCommand(command: ProcessCommand) = Future[Response] {
    command match {
      case command: StartProcess ⇒
        val id = newAgentProcessId()
        val process = newAgentProcess(toAgentProcessArguments(command, id))
        assert(process.id == id)
        idToProcess += process.id → process
        process.start()
        StartProcessResponse(process.id)

      case CloseProcess(id, kill) ⇒
        val process = idToProcess.remove(id) getOrElse throwUnknownProcess(id)
        if (kill) tryKillProcess(process)
        process.close()
        CloseProcessResponse
    }
  }
}

private object ProcessCommandExecutor {
  private val logger = Logger(getClass)

  private def toAgentProcessArguments(command: StartProcess, processId: AgentProcessId): AgentProcessArguments = {
    val startArguments = TaskStartArguments(controllerAddress = command.controllerAddress)
    command match {
      case _: StartThread ⇒ DummyProcessArguments(processId, startArguments)
      case o: StartDedicatedProcess ⇒ DedicatedProcessArguments(processId, startArguments,
        javaOptions = o.javaOptions, javaClasspath = o.javaClasspath)
    }
  }

  private def tryKillProcess(task: AgentProcess) =
    try task.kill()
    catch { case NonFatal(t) ⇒ logger.warn(s"Kill $task failed: $t") }

  private def throwUnknownProcess(id: AgentProcessId) = throw new NoSuchElementException(s"Unknown agent process '$id'")
}

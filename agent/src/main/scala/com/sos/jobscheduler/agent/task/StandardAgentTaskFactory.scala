package com.sos.jobscheduler.agent.task

import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.task.StandardAgentTaskFactory._
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.minicom.remoting.dialog.LocalServerDialogConnection
import com.sos.jobscheduler.taskserver.StandardTaskServer
import com.sos.jobscheduler.taskserver.data.TaskServerArguments
import com.sos.jobscheduler.taskserver.task.RemoteModuleInstanceServer
import java.net.InetAddress
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class StandardAgentTaskFactory @Inject private(
  agentConfiguration: AgentConfiguration,
  newRemoteModuleInstanceServer: RemoteModuleInstanceServer.Factory,
  agentTaskIdGenerator: AgentTaskId.Generator)
  (implicit
    timerService: TimerService,
    private val executionContext: ExecutionContext)
extends AgentTaskFactory {

  def apply(command: StartTask, clientIpOption: Option[InetAddress]): AgentTask = {
    val agentTaskId = agentTaskIdGenerator.next()
    val aTaskServer = newTaskServer(agentTaskId, command)
    closeOnError(aTaskServer) {
      new AgentTask {
        val jobPath = command.jobPath
        val id = agentTaskId
        val taskServer = aTaskServer
        protected val apiConnection = taskServer match {
          case taskServer: StandardTaskServer.LocalConnection ⇒
            new LocalApiConnection(taskServer.request, clientIpOption)
          //case _ ⇒ ???
        }
        def close() = closeApiConnectionAndTaskServer()
      }
    }
  }

  private def newTaskServer(agentTaskId: AgentTaskId, command: StartTask) = {
    val arguments = TaskServerArguments(
      agentTaskId,
      jobPath = command.jobPath,
      workingDirectory = agentConfiguration.workingDirectory,
      logDirectory = agentConfiguration.logDirectory,
      dotnet = agentConfiguration.dotnet,
      environment = agentConfiguration.environment,
      killScriptOption = agentConfiguration.killScript,
      rpcKeepaliveDurationOption = agentConfiguration.rpcKeepaliveDuration)
    if (runInProcess) {
      // For debugging
      logger.warn(s"Due to system property $InProcessName, task runs in Agent process")
      newLocalTaskServer(arguments)
    } else
      command match {
        case _: StartNonApiTask ⇒ newNoTcpLocalTaskServer(arguments)
        case _: StartApiTask ⇒ throw new NotImplementedError
      }
  }

  private def newLocalTaskServer(arguments: TaskServerArguments) =
    newNoTcpLocalTaskServer(arguments)

  // For non-C++ JobScheduler only.
  private def newNoTcpLocalTaskServer(taskServerArguments: TaskServerArguments) = {
    new StandardTaskServer with StandardTaskServer.LocalConnection {
      def arguments = taskServerArguments
      protected def executionContext = StandardAgentTaskFactory.this.executionContext
      protected val serverDialogConnection = new LocalServerDialogConnection()(executionContext)
      protected val newRemoteModuleInstanceServer = StandardAgentTaskFactory.this.newRemoteModuleInstanceServer
      def request = serverDialogConnection.leftSendAndReceive
      override def close() = {
        try serverDialogConnection.leftClose()
        finally super.close()
      }
    }
  }
}

object StandardAgentTaskFactory {
  private val logger = Logger(getClass)
  private val InProcessName = "jobscheduler.agent.inProcess"

  def runInProcess: Boolean = sys.props contains InProcessName
}

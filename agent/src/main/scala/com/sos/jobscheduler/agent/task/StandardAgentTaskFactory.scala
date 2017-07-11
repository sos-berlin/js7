package com.sos.jobscheduler.agent.task

import akka.agent.{Agent ⇒ AkkaAgent}
import akka.util.ByteString
import com.google.common.base.Splitter
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{StartApiTask, StartNonApiTask, StartTask}
import com.sos.jobscheduler.agent.task.StandardAgentTaskFactory._
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Collections.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.minicom.remoting.calls.{CallCall, ReleaseCall}
import com.sos.jobscheduler.minicom.remoting.dialog.LocalServerDialogConnection
import com.sos.jobscheduler.minicom.remoting.serial.CallDeserializer
import com.sos.jobscheduler.minicom.remoting.serial.CallDeserializer.deserializeCall
import com.sos.jobscheduler.minicom.types.VariantArray
import com.sos.jobscheduler.taskserver.data.TaskServerArguments
import com.sos.jobscheduler.taskserver.task.{RemoteModuleInstanceServer, TaskArguments}
import com.sos.jobscheduler.taskserver.{OwnProcessTaskServer, StandardTaskServer}
import com.sos.jobscheduler.tunnel.data.{TunnelId, TunnelToken}
import com.sos.jobscheduler.tunnel.server.{LocalTunnelHandle, TunnelListener, TunnelServer}
import java.net.InetAddress
import java.util.regex.Pattern
import javax.inject.{Inject, Provider, Singleton}
import scala.concurrent.{ExecutionContext, Future, Promise}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class StandardAgentTaskFactory @Inject private(
  agentConfiguration: AgentConfiguration,
  tunnelServerProvider: Provider[TunnelServer],
  newRemoteModuleInstanceServer: RemoteModuleInstanceServer.Factory,
  agentTaskIdGenerator: AgentTaskId.Generator)
  (implicit
    timerService: TimerService,
    private val executionContext: ExecutionContext)
extends AgentTaskFactory {

  private lazy val tunnelServer = tunnelServerProvider.get   // Initialize (and open TCP port) only when needed

  def apply(command: StartTask, clientIpOption: Option[InetAddress]): AgentTask = {
    val taskArgumentsPromise = Promise[TaskArguments]()
    val taskReleasePromise = Promise[Unit]()
    val agentTaskId = agentTaskIdGenerator.next()
    val aTunnelToken = TunnelToken.generate(TunnelId(agentTaskId.index.toString))
    val aTaskServer = newTaskServer(agentTaskId, command, aTunnelToken)
    closeOnError(aTaskServer) {
      new AgentTask {
        val id = agentTaskId
        val startMeta = command.meta getOrElse StartTask.Meta.Default
        val taskArgumentsFuture = taskArgumentsPromise.future
        val taskReleaseFuture = taskReleasePromise.future
        val taskServer = aTaskServer
        val tunnel = taskServer match {
          case taskServer: LocalConnection ⇒  // Only for the new non-C++ JobScheduler
            new LocalTunnelHandle(aTunnelToken, taskServer.request, clientIpOption)
          case _ ⇒
            tunnelServer.newTunnel(
              aTunnelToken,
              tunnelListener = AkkaAgent(new MyTunnelListener(taskArgumentsPromise, taskReleasePromise)),
              clientIpOption)
        }
        def close() = closeTunnelAndTaskServer()
      }
    }
  }

  private def newTaskServer(agentTaskId: AgentTaskId, command: StartTask, tunnelToken: TunnelToken) = {
    val arguments = TaskServerArguments(
      agentTaskId,
      startMeta = command.meta getOrElse StartTask.Meta.Default,
      masterAddress = "",
      tunnelToken = tunnelToken,
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
        case _: StartNonApiTask ⇒ newLocalTaskServer(arguments)
        case o: StartApiTask ⇒ new OwnProcessTaskServer(
          arguments.copy(masterAddress = tunnelServer.proxyAddressString),
          javaOptions = agentConfiguration.jobJavaOptions ++ splitJavaOptions(o.javaOptions),
          javaClasspath = o.javaClasspath)
      }
  }

  private def newLocalTaskServer(arguments: TaskServerArguments) =
    if (arguments.startMeta.taskId == StartTask.Meta.NoCppJobSchedulerTaskId)
      newNoTcpLocalTaskServer(arguments)
    else
      newTcpConnectedStandardTaskServer(arguments)

  // For non-C++ JobScheduler only.
  private def newNoTcpLocalTaskServer(taskServerArguments: TaskServerArguments) = {
    new StandardTaskServer with LocalConnection {
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

  private def newTcpConnectedStandardTaskServer(taskServerArguments: TaskServerArguments) =
    new StandardTaskServer with StandardTaskServer.TcpConnected {
      def arguments = taskServerArguments.copy(masterAddress = tunnelServer.proxyAddressString)
      protected val newRemoteModuleInstanceServer = StandardAgentTaskFactory.this.newRemoteModuleInstanceServer
      protected val executionContext = StandardAgentTaskFactory.this.executionContext
    }

  private def splitJavaOptions(options: String) =
    Splitter.on(Pattern.compile("\\s+")).trimResults.omitEmptyStrings.split(options).toImmutableSeq
}

object StandardAgentTaskFactory {
  private val logger = Logger(getClass)
  private val InProcessName = "jobscheduler.agent.inProcess"

  def runInProcess: Boolean = sys.props contains InProcessName

  private trait LocalConnection {
    private[StandardAgentTaskFactory] def request: ByteString ⇒ Future[ByteString]
  }

  /**
    * Resembles the behaviour of [[com.sos.jobscheduler.taskserver.task.RemoteModuleInstanceServer]].
    */
  private class MyTunnelListener(taskArgumentsPromise: Promise[TaskArguments], taskReleasePromise: Promise[Unit]) extends TunnelListener {
    def onRequest(msg: ByteString) =
      CallDeserializer.isCallCall(msg) option deserializeCall(msg) match {
        case Some(CallCall(constructProxyId, "construct", args)) ⇒  // The first call to a method "construct" must be for RemoteModuleInstanceServer
          taskArgumentsPromise.success(TaskArguments(args(0).asInstanceOf[VariantArray]))
          new TunnelListener {
            def onRequest(msg: ByteString) =
              CallDeserializer.isReleaseCall(msg) option deserializeCall(msg) match {
                case Some(ReleaseCall(`constructProxyId`)) ⇒
                  taskReleasePromise.success(())
                  this
                case _ ⇒
                  this
              }
          }
        case _ ⇒
          this
      }
  }
}

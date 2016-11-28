package com.sos.scheduler.engine.agent.task

import akka.agent.{Agent ⇒ AkkaAgent}
import akka.util.ByteString
import com.google.common.base.Splitter
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.commands.{StartApiTask, StartNonApiTask, StartTask}
import com.sos.scheduler.engine.agent.task.StandardAgentTaskFactory._
import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.common.scalautil.AutoClosing.closeOnError
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.remoting.calls.CallCall
import com.sos.scheduler.engine.minicom.remoting.serial.CallDeserializer.{deserializeCall, messageIsCall}
import com.sos.scheduler.engine.minicom.types.VariantArray
import com.sos.scheduler.engine.taskserver.data.TaskServerArguments
import com.sos.scheduler.engine.taskserver.task.{RemoteModuleInstanceServer, TaskArguments}
import com.sos.scheduler.engine.taskserver.{OwnProcessTaskServer, StandardTaskServer}
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.TunnelListener.StopListening
import com.sos.scheduler.engine.tunnel.server.{TunnelListener, TunnelServer}
import java.net.InetAddress
import java.util.regex.Pattern
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Promise}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class StandardAgentTaskFactory @Inject private(
  agentConfiguration: AgentConfiguration,
  tunnelServer: TunnelServer,
  newRemoteModuleInstanceServer: RemoteModuleInstanceServer.Factory)
  (implicit executionContext: ExecutionContext)
extends AgentTaskFactory {

  private val agentTaskIdGenerator = AgentTaskId.newGenerator()

  def apply(command: StartTask, clientIpOption: Option[InetAddress]): AgentTask = {
    val taskArgumentsPromise = Promise[TaskArguments]()
    new AgentTask {
      val id = agentTaskIdGenerator.next()
      val startMeta = command.meta getOrElse StartTask.Meta.Default
      val taskArgumentsFuture = taskArgumentsPromise.future
      val tunnel = {
        val listener = AkkaAgent(new TaskArgumentsListener(taskArgumentsPromise))
        tunnelServer.newTunnel(TunnelId(id.index.toString), listener, clientIpOption)
      }
      val taskServer = closeOnError(tunnel) {
        newTaskServer(id, command, masterAddress = tunnelServer.proxyAddressString, tunnel.tunnelToken)
      }

      def close() = closeTunnelAndTaskServer()
    }
  }

  private def newTaskServer(agentTaskId: AgentTaskId, command: StartTask, masterAddress: String, tunnelToken: TunnelToken) = {
    val arguments = TaskServerArguments(
      agentTaskId,
      startMeta = command.meta getOrElse StartTask.Meta.Default,
      masterAddress = masterAddress,
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
      newStandardTaskServer(arguments)
    } else
      command match {
        case _: StartNonApiTask ⇒ newStandardTaskServer(arguments)
        case o: StartApiTask ⇒ new OwnProcessTaskServer(
          arguments,
          javaOptions = agentConfiguration.jobJavaOptions ++ splitJavaOptions(o.javaOptions),
          javaClasspath = o.javaClasspath)
      }
  }

  private def newStandardTaskServer(arguments: TaskServerArguments) =
    new StandardTaskServer(newRemoteModuleInstanceServer, arguments)

  private def splitJavaOptions(options: String) =
    Splitter.on(Pattern.compile("\\s+")).trimResults.omitEmptyStrings.split(options).toImmutableSeq
}

object StandardAgentTaskFactory {
  private val logger = Logger(getClass)
  private val InProcessName = "jobscheduler.agent.inProcess"

  def runInProcess: Boolean = sys.props contains InProcessName

  /**
   * Resembles the behaviour of [[com.sos.scheduler.engine.taskserver.task.RemoteModuleInstanceServer]]#invoke.
   */
  private class TaskArgumentsListener(promise: Promise[TaskArguments]) extends TunnelListener {
    def onRequest(msg: ByteString) =
      messageIsCall(msg) option deserializeCall(msg) match {
        case Some(CallCall(_, "construct", args)) ⇒
          promise.success(TaskArguments(args(0).asInstanceOf[VariantArray]))
          StopListening
        case _ ⇒ this
      }
  }
}

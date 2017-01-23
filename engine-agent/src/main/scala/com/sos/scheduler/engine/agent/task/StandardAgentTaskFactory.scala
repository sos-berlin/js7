package com.sos.scheduler.engine.agent.task

import akka.actor.ActorSystem
import akka.agent.{Agent ⇒ AkkaAgent}
import akka.util.ByteString
import com.google.common.base.Splitter
import com.google.inject.Injector
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.commands.{StartApiTask, StartNonApiTask, StartTask}
import com.sos.scheduler.engine.agent.task.StandardAgentTaskFactory._
import com.sos.scheduler.engine.common.scalautil.AutoClosing.closeOnError
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.minicom.remoting.calls.{CallCall, ReleaseCall}
import com.sos.scheduler.engine.minicom.remoting.serial.CallDeserializer
import com.sos.scheduler.engine.minicom.remoting.serial.CallDeserializer.deserializeCall
import com.sos.scheduler.engine.minicom.types.VariantArray
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments
import com.sos.scheduler.engine.taskserver.task.TaskArguments
import com.sos.scheduler.engine.taskserver.{OwnProcessTaskServer, SimpleTaskServer}
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.{TunnelListener, TunnelServer}
import java.net.InetAddress
import java.util.regex.Pattern
import javax.inject.{Inject, Singleton}
import scala.concurrent.Promise

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class StandardAgentTaskFactory @Inject private(
  agentConfiguration: AgentConfiguration,
  tunnelServer: TunnelServer,
  actorSystem: ActorSystem,
  injector: Injector)
extends AgentTaskFactory {

  import actorSystem.dispatcher

  private val agentTaskIdGenerator = AgentTaskId.newGenerator()

  def apply(command: StartTask, clientIpOption: Option[InetAddress]): AgentTask = {
    val taskArgumentsPromise = Promise[TaskArguments]()
    val taskReleasePromise = Promise[Unit]()
    new AgentTask {
      val id = agentTaskIdGenerator.next()
      val startMeta = command.meta getOrElse StartTask.Meta.Default
      val taskArgumentsFuture = taskArgumentsPromise.future
      val taskReleaseFuture = taskReleasePromise.future
      val tunnel = {
        val listener = AkkaAgent(new MyTunnelListener(taskArgumentsPromise, taskReleasePromise))
        tunnelServer.newTunnel(TunnelId(id.index.toString), listener, clientIpOption)
      }
      val taskServer = closeOnError(tunnel) {
        newTaskServer(id, command, masterAddress = tunnelServer.proxyAddressString, tunnel.tunnelToken)
      }

      def close() = closeTunnelAndTaskServer()
    }
  }

  private def newTaskServer(agentTaskId: AgentTaskId, command: StartTask, masterAddress: String, tunnelToken: TunnelToken) = {
    val taskStartArguments = TaskStartArguments(
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
      new SimpleTaskServer(injector, taskStartArguments)
    } else
      command match {
        case _: StartNonApiTask ⇒ new SimpleTaskServer(injector, taskStartArguments)
        case o: StartApiTask ⇒ new OwnProcessTaskServer(
          taskStartArguments,
          javaOptions = agentConfiguration.jobJavaOptions ++ splitJavaOptions(o.javaOptions),
          javaClasspath = o.javaClasspath)
      }
  }

  private def splitJavaOptions(options: String) =
    Splitter.on(Pattern.compile("\\s+")).trimResults.omitEmptyStrings.split(options).toImmutableSeq
}

object StandardAgentTaskFactory {
  private val logger = Logger(getClass)
  private val InProcessName = "jobscheduler.agent.inProcess"

  def runInProcess = sys.props contains InProcessName

  /**
    * Resembles the behaviour of [[com.sos.scheduler.engine.taskserver.task.RemoteModuleInstanceServer]].
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

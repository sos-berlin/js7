package com.sos.scheduler.engine.agent.main

import com.sos.scheduler.engine.agent.Agent
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.data.commands.Terminate
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Closers.EmptyAutoCloseable
import com.sos.scheduler.engine.common.scalautil.Futures.awaitResult
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.system.FileUtils.temporaryDirectory
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.JavaShutdownHook
import com.sos.scheduler.engine.taskserver.dotnet.DotnetEnvironment
import scala.util.control.NonFatal

/**
 * JobScheduler Agent.
 *
 * @author Joacim Zschimmer
 */
object AgentMain {
  private val logger = Logger(getClass)
  private val OnJavaShutdownSigkillProcessesAfter = 5.s
  private val ShutdownTimeout = OnJavaShutdownSigkillProcessesAfter + 2.s

  def main(args: Array[String]): Unit = {
    val (conf, environment) = tryProvideDotnet(AgentConfiguration(args))
    try autoClosing(environment) { _ ⇒
      run(conf)
    }
    catch { case NonFatal(t) ⇒
      println(s"JOBSCHEDULER AGENT TERMINATED DUE TO ERROR: $t")
      logger.error(t.toString, t)
      System.exit(1)
    }
  }

  private def tryProvideDotnet(conf: AgentConfiguration): (AgentConfiguration, AutoCloseable) =
    conf match {
      case c if isWindows ⇒
        val env = new DotnetEnvironment(temporaryDirectory)
        (c withDotnetAdapterDirectory Some(env.directory), env)
      case c ⇒ (c, EmptyAutoCloseable)
    }

  def run(conf: AgentConfiguration): Unit =
    autoClosing(new Agent(conf)) { agent ⇒
      def onShutdown(): Unit = {
        agent.executeCommand(Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(OnJavaShutdownSigkillProcessesAfter)))
        awaitResult(agent.terminated, ShutdownTimeout)
      }
      autoClosing(JavaShutdownHook.add(onShutdown, name = AgentMain.getClass.getName)) { _ ⇒
        agent.run()
      }
    }
}

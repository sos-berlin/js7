package com.sos.jobscheduler.agent.main

import com.sos.jobscheduler.agent.Agent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.Terminate
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.Closers.{EmptyAutoCloseable, withCloser}
import com.sos.jobscheduler.common.scalautil.Futures.awaitResult
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import com.sos.jobscheduler.taskserver.dotnet.DotnetEnvironment
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
    withCloser { implicit closer ⇒
      val agent = new Agent(conf).closeWithCloser
      JavaShutdownHook.add("AgentMain") {
        agent.executeCommand(Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(OnJavaShutdownSigkillProcessesAfter)))
        awaitResult(agent.terminated, ShutdownTimeout)
        closer.close()
        Log4j.shutdown()
      }.closeWithCloser
      agent.run()
    }
}

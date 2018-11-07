package com.sos.jobscheduler.agent.main

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Terminate
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.DurationInt

/**
 * JobScheduler Agent.
 *
 * @author Joacim Zschimmer
 */
object AgentMain {
  private val logger = Logger(getClass)
  private val OnJavaShutdownSigkillProcessesAfter = 5.seconds
  private val ShutdownTimeout = OnJavaShutdownSigkillProcessesAfter + 2.seconds

  def main(args: Array[String]): Unit = {
    logger.info(s"Agent ${BuildInfo.buildVersion}")  // Log early
    try {
      val agentConfiguration = AgentConfiguration.fromCommandLine(args.toVector)
      autoClosing(RunningAgent(agentConfiguration).awaitInfinite) { agent ⇒
        autoClosing(JavaShutdownHook.add("AgentMain")(onJavaShutdown(agent))) { _ ⇒  // TODO Interfers with Akkas CoordinatedShutdown shutdown hook ?
          agent.terminated.awaitInfinite
        }
      }
      val msg = "JobScheduler Agent terminates"
      logger.info(msg)
      Log4j.shutdown()
      println(msg)
    }
    catch { case t: Throwable ⇒
      logger.error(t.toString, t)
      Log4j.shutdown()
      println(s"JOBSCHEDULER AGENT TERMINATES DUE TO ERROR: ${t.toStringWithCauses}")
      sys.runtime.exit(1)
    }
  }

  private def onJavaShutdown(agent: RunningAgent): Unit =
    try {
      logger.warn("Trying to terminate Agent due to Java shutdown")
      agent.executeCommand(Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(OnJavaShutdownSigkillProcessesAfter))).runAsync
      agent.terminated await ShutdownTimeout.toJavaDuration
      agent.close()
    } finally
      Log4j.shutdown()
}

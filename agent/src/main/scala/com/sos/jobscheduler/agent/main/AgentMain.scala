package com.sos.jobscheduler.agent.main

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Terminate
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.JavaMainSupport.{handleJavaShutdown, runMain}
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.Duration

/**
 * JobScheduler Agent.
 *
 * @author Joacim Zschimmer
 */
object AgentMain {
  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    logger.info(s"Agent ${BuildInfo.buildVersion}")  // Log early for early timestamp and propery logger initialization by a single (not-parallel) call
    ProblemCodeMessages.initialize()
    runMain {
      val agentConfiguration = AgentConfiguration.fromCommandLine(args.toVector)
      autoClosing(RunningAgent(agentConfiguration).awaitInfinite) { agent ⇒
        handleJavaShutdown(agentConfiguration.config, "AgentMain", onJavaShutdown(agent)) {
          agent.terminated.awaitInfinite
        }
      }
      val msg = "JobScheduler Agent terminates"
      logger.info(msg)
      println(msg)
    }
  }

  private def onJavaShutdown(agent: RunningAgent)(timeout: Duration): Unit = {
    logger.warn("Trying to terminate Agent due to Java shutdown")
    val sigkillAfter = agent.config.getDuration("jobscheduler.termination.sigkill-after").toFiniteDuration
    agent.executeCommand(Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(sigkillAfter))).runAsync
    agent.terminated await timeout
    agent.close()
  }
}

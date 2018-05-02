package com.sos.jobscheduler.agent.main

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Terminate
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Closers.EmptyAutoCloseable
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
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
    val exitCode = try {
      logger.info(s"Agent ${BuildInfo.buildVersion}")  // Log early
      val agentConfiguration = AgentConfiguration(args)
      val (conf, dotnet) = tryProvideDotnet(agentConfiguration)
      try run(conf).awaitInfinite
      finally dotnet.close()
      val msg = "JobScheduler Agent terminated"
      println(msg)
      logger.debug(msg)
      0
    }
    catch { case t: Throwable ⇒
      logger.error(t.toString, t)
      println(t.toStringWithCauses)
      println("JOBSCHEDULER AGENT TERMINATED DUE TO ERROR")
      1
    }
    Log4j.shutdown()
    sys.runtime.exit(exitCode)
  }

  private def tryProvideDotnet(conf: AgentConfiguration): (AgentConfiguration, AutoCloseable) =
    conf match {
      // TODO DotnetModule als ServiceProvider realisieren, so dass unter Unix kompilierter Agent auch unter Windows nutzbar ist.
      //case c if isWindows ⇒
      //  val env = new DotnetEnvironment(temporaryDirectory)
      //  (c withDotnetAdapterDirectory Some(env.directory), env)
      case c ⇒ (c, EmptyAutoCloseable)
    }

  private def run(conf: AgentConfiguration): Future[Completed] = {
    (for (agent ← RunningAgent(conf)) yield {
      val hook = JavaShutdownHook.add("AgentMain") {
        // TODO Interfers with Akkas CoordinatedShutdown shutdown hook
        onJavaShutdown(agent)
      }
      agent.terminated andThen { case _ ⇒
        hook.remove()
      }
    }).flatten
  }

  private def onJavaShutdown(agent: RunningAgent): Unit = {
    logger.info("Terminating Agent due to Java shutdown")
    agent.commandHandler.execute(Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(OnJavaShutdownSigkillProcessesAfter)))
    agent.terminated await ShutdownTimeout.toJavaDuration
    agent.close()
    Log4j.shutdown()
  }
}

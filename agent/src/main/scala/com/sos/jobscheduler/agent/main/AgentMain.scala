package com.sos.jobscheduler.agent.main

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Terminate
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Closers.EmptyAutoCloseable
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import com.sos.jobscheduler.taskserver.dotnet.DotnetEnvironment
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * JobScheduler Agent.
 *
 * @author Joacim Zschimmer
 */
object AgentMain {
  private val logger = Logger(getClass)
  private val OnJavaShutdownSigkillProcessesAfter = 5.s
  private val ShutdownTimeout = OnJavaShutdownSigkillProcessesAfter + 2.s

  def main(args: Array[String]): Unit =
    try {
      val agentConfiguration = AgentConfiguration(args)
      val (conf, dotnet) = tryProvideDotnet(agentConfiguration)
      start(conf) andThen { case _ ⇒
        dotnet.close()
      } onComplete {
        case Success(Completed) ⇒
          println("JobScheduler Agent terminated")
          logger.debug("Terminated")
          Log4j.shutdown()
          sys.runtime.exit(0)
        case Failure(t) ⇒
          exitJava(t)
      }
    }
    catch { case t: Throwable ⇒
      exitJava(t)
    }

  private def tryProvideDotnet(conf: AgentConfiguration): (AgentConfiguration, AutoCloseable) =
    conf match {
      case c if isWindows ⇒
        val env = new DotnetEnvironment(temporaryDirectory)
        (c withDotnetAdapterDirectory Some(env.directory), env)
      case c ⇒ (c, EmptyAutoCloseable)
    }

  def start(conf: AgentConfiguration): Future[Completed] = {
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

  private def exitJava(throwable: Throwable): Unit = {
    println(s"JOBSCHEDULER AGENT TERMINATED DUE TO ERROR: ${throwable.toStringWithCauses}")
    logger.error(throwable.toString, throwable)
    Log4j.shutdown()
    sys.runtime.exit(1)
  }

  private def onJavaShutdown(agent: RunningAgent): Unit = {
    logger.info("Terminating Agent due to Java shutdown")
    agent.commandHandler.execute(Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(OnJavaShutdownSigkillProcessesAfter)))
    agent.terminated await ShutdownTimeout
    agent.close()
    Log4j.shutdown()
  }
}

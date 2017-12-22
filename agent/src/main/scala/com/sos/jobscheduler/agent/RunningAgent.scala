package com.sos.jobscheduler.agent

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.model.Uri
import com.google.common.io.Closer
import com.google.inject
import com.google.inject.Stage.PRODUCTION
import com.google.inject.{Guice, Injector, Module}
import com.sos.jobscheduler.agent.command.CommandHandler
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.views.AgentStartInformation
import com.sos.jobscheduler.agent.web.AgentWebServer
import com.sos.jobscheduler.agent.web.common.LoginSession
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRegister
import com.sos.jobscheduler.common.guice.GuiceImplicits._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.Duration
import org.jetbrains.annotations.TestOnly
import scala.concurrent.{ExecutionContext, Future, Promise, blocking}
import scala.util.Failure

/**
 * JobScheduler Agent.
 *
 * Integration test in engine-tests, for example com.sos.jobscheduler.tests.jira.js1291.JS1291AgentIT.
 *
 * @author Joacim Zschimmer
 */
final class RunningAgent private(
  val webServer: AgentWebServer,
  val terminated: Future[Completed],
  val commandHandler: CommandHandler,
  closer: Closer,
  @TestOnly val injector: Injector)
extends AutoCloseable {

  val localUri: Uri = webServer.localUri

  def close() = closer.close()
}

object RunningAgent {
  private val logger = Logger(getClass)
  private val WebServerReadyTimeout = 60.s

  def run[A](configuration: AgentConfiguration, timeout: Option[Duration] = None)(body: RunningAgent ⇒ A): A =
    autoClosing(apply(configuration) await timeout) { agent ⇒
      implicit val executionContext = agent.injector.instance[ExecutionContext]
      val a = body(agent)
      agent.terminated await 99.s
      a
    }

  def apply(configuration: AgentConfiguration): Future[RunningAgent] =
    apply(new AgentModule(configuration))

  def apply(module: Module): Future[RunningAgent] = {
    AgentStartInformation.initialize()
    val injector = Guice.createInjector(PRODUCTION, module)
    val agentConfiguration = injector.instance[AgentConfiguration]
    logger.info(s"Agent ${BuildInfo.buildVersion} config=${agentConfiguration.configDirectory getOrElse ""} data=${agentConfiguration.dataDirectory getOrElse ""}")

    implicit val executionContext = injector.instance[ExecutionContext]
    implicit val actorSystem = injector.instance[ActorSystem]
    val closer = injector.instance[Closer]
    val webServer = injector.instance[AgentWebServer]
    val webServerReady = webServer.start()
    val sessionRegister = injector.getInstance(inject.Key.get(new inject.TypeLiteral[SessionRegister[LoginSession]] {}))
    val readyPromise = Promise[MainActor.Ready]()
    val stoppedPromise = Promise[Completed]()
    actorSystem.actorOf(
      Props { new MainActor(agentConfiguration, sessionRegister, injector, readyPromise, stoppedPromise) },
      "main")
    for (ready ← readyPromise.future) yield {
      webServerReady await WebServerReadyTimeout
      webServer.setCommandHandler(ready.commandHandler)
      webServer.setAgentActor(ready.agentHandle)
      val terminated = stoppedPromise.future andThen { case _ ⇒
        blocking {
          logger.debug("Delaying close to let HTTP server respond open requests")
          sleep(500.ms)
        }
        closer.close()  // Close automatically after termination
      } andThen {
        case Failure(t) ⇒ logger.error(t.toStringWithCauses, t)
      }
      new RunningAgent(webServer, terminated, ready.commandHandler, closer, injector)
    }
  }
}

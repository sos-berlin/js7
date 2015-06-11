package com.sos.scheduler.engine.agent

import com.google.common.io.Closer
import com.google.inject.Guice
import com.google.inject.Stage.PRODUCTION
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.inject.AgentModule
import com.sos.scheduler.engine.agent.process.ProcessHandler
import com.sos.scheduler.engine.agent.web.AgentWebServer
import com.sos.scheduler.engine.common.guice.GuiceImplicits._
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Futures.awaitResult
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder._
import scala.concurrent.Future
import scala.concurrent.duration.Duration

/**
 * JobScheduler Agent.
 *
 * Integration test in engine-tests, for example com.sos.scheduler.engine.tests.jira.js1291.JS1291AgentIT.
 *
 * @author Joacim Zschimmer
 */
final class Agent(configuration: AgentConfiguration) extends AutoCloseable {

  val localUri = s"http://127.0.0.1:${configuration.httpPort}"
  private val injector = Guice.createInjector(PRODUCTION, new AgentModule(configuration))
  private val server = injector.instance[AgentWebServer]
  private val closer = injector.instance[Closer]
  private val processHandler = injector.instance[ProcessHandler]

  def start(): Future[Unit] = server.start()

  def close(): Unit = closer.close()

  def terminated: Future[Unit] = processHandler.terminated
}

object Agent {
  def main(args: Array[String]): Unit = run(AgentConfiguration(args))

  def run(conf: AgentConfiguration): Unit =
    autoClosing(new Agent(conf)) { agent ⇒
      agent.start()
      awaitResult(agent.terminated, Duration.Inf)
    }

  def forTest(): Agent = forTest(httpPort = findRandomFreeTcpPort())

  def forTest(httpPort: Int): Agent =
    new Agent(AgentConfiguration(
      httpPort = httpPort,
      httpInterfaceRestriction = Some("127.0.0.1")))
}

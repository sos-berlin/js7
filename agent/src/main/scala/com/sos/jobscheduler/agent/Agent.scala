package com.sos.jobscheduler.agent

import com.google.common.io.Closer
import com.google.inject.Stage.PRODUCTION
import com.google.inject.{Guice, Injector, Module}
import com.sos.jobscheduler.agent.Agent._
import com.sos.jobscheduler.agent.command.CommandExecutor
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Response
import com.sos.jobscheduler.agent.scheduler.OrderHandler
import com.sos.jobscheduler.agent.views.AgentStartInformation
import com.sos.jobscheduler.agent.web.AgentWebServer
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.guice.GuiceImplicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder._
import com.sos.jobscheduler.data.agent.AgentAddress
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

/**
 * JobScheduler Agent.
 *
 * Integration test in engine-tests, for example com.sos.jobscheduler.tests.jira.js1291.JS1291AgentIT.
 *
 * @author Joacim Zschimmer
 */
@Singleton
final class Agent @Inject private(
  val configuration: AgentConfiguration,
  webServer: AgentWebServer,
  commandExecutor: CommandExecutor,
  orderHandler: OrderHandler,
  val injector: Injector)
  (implicit closer: Closer, executionContext: ExecutionContext)
extends AutoCloseable {

  val localUri = AgentAddress(webServer.localUri.toString)

  AgentStartInformation.initialize()

  def start(): Future[Unit] = {
    logger.info(s"Agent ${BuildInfo.buildVersion} config=${configuration.configDirectory getOrElse ""} data=${configuration.dataDirectory getOrElse ""}")
    webServer.start()
  }

  def close(): Unit = {
    logger.info("close")
    closer.close()
    logger.info("closed")
  }

  def run(): Unit = {
    start() await 30.s
    terminated.awaitInfinite
  }

  def executeCommand(command: AgentCommand): Future[Response] =
    commandExecutor.executeCommand(command)

  def terminated: Future[Completed] =
    orderHandler.terminated
}

object Agent {
  private val logger = Logger(getClass)

  def apply(configuration: AgentConfiguration): Agent =
    apply(new AgentModule(configuration))

  def apply(module: Module): Agent =
    Guice.createInjector(PRODUCTION, module).instance[Agent]

  def forTest(): Agent = forTest(httpPort = findRandomFreeTcpPort())

  def forTest(httpPort: Int): Agent = Agent(AgentConfiguration.forTest(httpPort = httpPort))
}

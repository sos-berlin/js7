package com.sos.jobscheduler.agent

import com.google.common.io.Closer
import com.google.inject.Stage.PRODUCTION
import com.google.inject.{Guice, Module}
import com.sos.jobscheduler.agent.Agent._
import com.sos.jobscheduler.agent.command.CommandExecutor
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.data.commandresponses.Response
import com.sos.jobscheduler.agent.data.commands.Command
import com.sos.jobscheduler.agent.task.TaskHandler
import com.sos.jobscheduler.agent.views.AgentStartInformation
import com.sos.jobscheduler.agent.web.AgentWebServer
import com.sos.jobscheduler.common.guice.GuiceImplicits._
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder._
import com.sos.jobscheduler.data.agent.AgentAddress
import scala.concurrent.Future

/**
 * JobScheduler Agent.
 *
 * Integration test in engine-tests, for example com.sos.jobscheduler.tests.jira.js1291.JS1291AgentIT.
 *
 * @author Joacim Zschimmer
 */
final class Agent(module: Module) extends AutoCloseable {

  def this(configuration: AgentConfiguration) = this(new AgentModule(configuration))

  val injector = Guice.createInjector(PRODUCTION, module)
  val configuration = injector.instance[AgentConfiguration]
  private implicit val closer = injector.instance[Closer]
  private val webServer = injector.instance[AgentWebServer].closeWithCloser
  val localUri = AgentAddress(webServer.localUri.toString)
  private val taskHandler = injector.instance[TaskHandler]
  private val commandExecutor = injector.instance[CommandExecutor]

  AgentStartInformation.initialize()

  def start(): Future[Unit] = webServer.start()

  def close(): Unit = {
    logger.info("close")
    closer.close()
    logger.info("closed")
  }

  def run(): Unit = {
    start() await 30.s
    terminated.awaitInfinite
  }

  def executeCommand(command: Command): Future[Response] = commandExecutor.executeCommand(command)

  def terminated: Future[Unit] = taskHandler.terminated
}

object Agent {
  private val logger = Logger(getClass)

  def forTest(): Agent = forTest(httpPort = findRandomFreeTcpPort())

  def forTest(httpPort: Int): Agent = new Agent(AgentConfiguration.forTest(httpPort = httpPort))
}

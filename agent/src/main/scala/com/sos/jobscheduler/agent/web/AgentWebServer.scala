package com.sos.jobscheduler.agent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes.ServiceUnavailable
import com.google.common.io.Closer
import com.google.inject.Injector
import com.sos.jobscheduler.agent.command.CommandHandler
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.agent.web.AgentWebServer._
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.akkahttp.HttpStatusCodeException
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.time.timer.TimerService
import monix.execution.Scheduler
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
final class AgentWebServer(
  conf: AgentConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
  timerService: TimerService,
  closer: Closer,
  injector: Injector)
  (implicit
    protected val actorSystem: ActorSystem,
    protected val executionContext: ExecutionContext)
extends AkkaWebServer with AkkaWebServer.HasUri {

  closer.registerAutoCloseable(this)

  protected val bindings = conf.webServerBindings.toVector
  private val commandHandler = new SetOnce[CommandHandler]("CommandHandler")
  private val agentHandle = new SetOnce[AgentHandle]("Agent")

  def setCommandHandler(commandHandler: CommandHandler) =
    this.commandHandler := commandHandler

  def setAgentActor(agentHandle: AgentHandle) =
    this.agentHandle := agentHandle

  protected def newRoute(binding: WebServerBinding) =
    new CompleteRoute {
      protected val gateKeeper = new GateKeeper(gateKeeperConfiguration, timerService,
        isLoopback = binding.address.getAddress.isLoopbackAddress,
        mutual = binding.mutual)
      protected def sessionRegister = injector.instance[SessionRegister[SimpleSession]]
      //protected val taskRegister = Factory.this.taskRegister
      protected def commandHandler = AgentWebServer.this.commandHandler getOrElse (throw ServiceUnavailableException)
      protected def agentOverview = agentHandle.overview
      protected def agentHandle = AgentWebServer.this.agentHandle getOrElse (throw ServiceUnavailableException)
      protected def timerService = AgentWebServer.this.timerService
      protected def akkaAskTimeout = conf.akkaAskTimeout
      protected def config = AgentWebServer.this.conf.config
      protected def actorSystem = AgentWebServer.this.actorSystem
      protected def scheduler = injector.instance[Scheduler]

      logger.info(gateKeeper.boundMessage(binding))
    }.completeRoute
}

object AgentWebServer {
  private val logger = Logger(getClass)

  private val ServiceUnavailableException =
    new HttpStatusCodeException(ServiceUnavailable, Problem("Agent is not yet completely ready"))
}

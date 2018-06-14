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
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.{HttpStatusCodeException, WebServerBinding}
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.time.timer.TimerService
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

  protected def newRoute(binding: WebServerBinding) = {  // TODO Sollte aussehen wie in MasterWebServer
    val gateKeeper = new GateKeeper(gateKeeperConfiguration, timerService, isLoopback = binding.address.getAddress.isLoopbackAddress)
    val result = injector.instance[RouteProvider.Factory].toRoute(
      gateKeeper,
      () ⇒ commandHandler getOrElse newServiceUnavailable(),
      () ⇒ agentHandle getOrElse newServiceUnavailable())
    logger.info(gateKeeper.boundMessage(binding.address, binding.scheme))
    result
  }
}

object AgentWebServer {
  private val logger = Logger(getClass)

  private def newServiceUnavailable() =
    throw new HttpStatusCodeException(ServiceUnavailable, Problem("Agent CommandHandler is not yet ready"))
}

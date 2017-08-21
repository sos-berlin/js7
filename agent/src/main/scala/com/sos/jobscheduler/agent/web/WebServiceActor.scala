package com.sos.jobscheduler.agent.web

import akka.actor.{ActorSystem, Props}
import com.google.inject.Injector
import com.sos.jobscheduler.agent.command.{AgentCommandHandler, CommandExecutor, CommandMeta}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.OrderHandler
import com.sos.jobscheduler.agent.views.AgentOverview
import com.sos.jobscheduler.agent.web.WebServiceActor._
import com.sos.jobscheduler.agent.web.common.{ExternalWebService, LoginSession}
import com.sos.jobscheduler.agent.web.views.{CommandViewWebService, RootWebService}
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.sprayutils.web.auth.GateKeeper
import com.sos.jobscheduler.common.sprayutils.web.session.SessionRegister
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.session.SessionToken
import javax.inject.{Inject, Provider, Singleton}
import scala.collection.immutable
import scala.concurrent.ExecutionContext
import spray.routing._

/**
 * @author Joacim Zschimmer
 */
// An Actor must not be a @Singleton!
final private class WebServiceActor private(
  gateKeeper: GateKeeper,
  commandExecutor: CommandExecutor,
  agentOverviewProvider: Provider[AgentOverview],
  protected val sessionRegister: SessionRegister[LoginSession],
  protected val commandHandler: AgentCommandHandler,
  protected val orderHandler: OrderHandler,
  protected val timerService: TimerService,
  extraWebServices: immutable.Seq[ExternalWebService],
  agentConfiguration: AgentConfiguration,
  protected val eventIdGenerator: EventIdGenerator,
  protected val actorSystem: ActorSystem,
  implicit protected val executionContext: ExecutionContext,
  injector: Injector)
extends HttpServiceActor
with TimerWebService
with CommandWebService
with MastersEventWebService
with OrderWebService
with RootWebService
with CommandViewWebService
{
  protected val uriPathPrefix = agentConfiguration.uriPathPrefix
  protected def akkaAskTimeout = agentConfiguration.akkaAskTimeout

  protected def sessionTokenIsValid(sessionToken: SessionToken) = sessionRegister contains sessionToken

  protected def commandHandlerOverview = commandHandler
  protected def commandRunOverviews = commandHandler.commandRuns
  protected def executeCommand(command: AgentCommand, meta: CommandMeta) = commandExecutor.executeCommand(command, meta)
  protected def agentOverview = agentOverviewProvider.get()
  protected def config = agentConfiguration.config

  for (o ← extraWebServices) {
    logger.debug(s"Adding extra route $o")
    routeBuilder ++= o.routeBuilder
  }

  def receive = runRoute {
    buildRoute(gateKeeper)
  }
}

private[web] object WebServiceActor {
  private val logger = Logger(getClass)

  private[web] def props(gateKeeper: GateKeeper, injector: Injector) =
    Props { injector.instance[Factory].apply(gateKeeper) }

  @Singleton
  private class Factory @Inject private(
    commandExecutor: CommandExecutor,
    agentOverviewProvider: Provider[AgentOverview],
    sessionRegister: SessionRegister[LoginSession],
    commandHandler: AgentCommandHandler,
    orderHandler: OrderHandler,
    timerService: TimerService,
    extraWebServices: immutable.Seq[ExternalWebService],
    agentConfiguration: AgentConfiguration,
    eventIdGenerator: EventIdGenerator,
    actorSystem: ActorSystem,
    executionContext: ExecutionContext,
    injector: Injector)
  {
    def apply(gateKeeper: GateKeeper) =
      new WebServiceActor(
        gateKeeper,
        commandExecutor,
        agentOverviewProvider,
        sessionRegister,
        commandHandler,
        orderHandler,
        timerService,
        extraWebServices,
        agentConfiguration,
        eventIdGenerator,
        actorSystem,
        executionContext,
        injector)
  }
}

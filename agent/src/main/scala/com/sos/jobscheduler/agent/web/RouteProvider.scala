package com.sos.jobscheduler.agent.web

import akka.actor.{ActorRefFactory, ActorSystem}
import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import com.sos.jobscheduler.agent.command.CommandHandler
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.agent.web.RouteProvider._
import com.sos.jobscheduler.agent.web.common.{ExternalWebService, LoginSession}
import com.sos.jobscheduler.agent.web.views.RootWebService
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRegister
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import javax.inject.{Inject, Singleton}
import scala.collection.immutable
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
private abstract class RouteProvider private(
  gateKeeper: GateKeeper,
  protected val sessionRegister: SessionRegister[LoginSession],
  protected val timerService: TimerService,
  extraWebServices: immutable.Seq[ExternalWebService],
  agentConfiguration: AgentConfiguration,
  protected val eventIdGenerator: EventIdGenerator,
  protected val actorSystem: ActorSystem,
  implicit protected val executionContext: ExecutionContext,
  injector: Injector)
extends RootWebService
  with CommandWebService
  with MastersEventWebService
  with OrderWebService
  with TimerWebService
{
  protected val uriPathPrefix = agentConfiguration.uriPathPrefix
  protected def akkaAskTimeout = agentConfiguration.akkaAskTimeout
  protected def agentOverview = agentHandle.overview
  protected def config = agentConfiguration.config

  for (o ← extraWebServices) {
    logger.debug(s"Adding extra route $o")
    routeBuilder ++= o.routeBuilder
  }

  def route(implicit actorRefFactory: ActorRefFactory): Route =
    buildRoute(gateKeeper)
}

private[web] object RouteProvider {
  private val logger = Logger(getClass)

  @Singleton
  final class Factory @Inject private(
    sessionRegister: SessionRegister[LoginSession],
    timerService: TimerService,
    extraWebServices: immutable.Seq[ExternalWebService],
    agentConfiguration: AgentConfiguration,
    eventIdGenerator: EventIdGenerator,
    actorSystem: ActorSystem,
    executionContext: ExecutionContext,
    injector: Injector)
  {
    def toRoute(gateKeeper: GateKeeper, getCommandHandler: () ⇒ CommandHandler, getAgentHandle: () ⇒ AgentHandle): Route =
      new RouteProvider(
        gateKeeper,
        sessionRegister,
        timerService,
        extraWebServices,
        agentConfiguration,
        eventIdGenerator,
        actorSystem,
        executionContext,
        injector)
      {
        protected def commandHandler = getCommandHandler()
        protected def agentHandle = getAgentHandle()
      }.route(actorSystem)
  }
}

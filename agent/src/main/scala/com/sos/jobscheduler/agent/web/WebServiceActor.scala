package com.sos.jobscheduler.agent.web

import akka.actor.{ActorSystem, Props}
import com.google.inject.Injector
import com.sos.jobscheduler.agent.command.CommandHandler
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.agent.web.WebServiceActor._
import com.sos.jobscheduler.agent.web.common.{ExternalWebService, LoginSession}
import com.sos.jobscheduler.agent.web.views.RootWebService
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.sprayutils.web.auth.GateKeeper
import com.sos.jobscheduler.common.sprayutils.web.session.SessionRegister
import com.sos.jobscheduler.common.time.timer.TimerService
import javax.inject.{Inject, Singleton}
import scala.collection.immutable
import scala.concurrent.ExecutionContext
import spray.routing._

/**
 * @author Joacim Zschimmer
 */
private abstract class WebServiceActor private(
  gateKeeper: GateKeeper,
  protected val sessionRegister: SessionRegister[LoginSession],
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
{
  protected val uriPathPrefix = agentConfiguration.uriPathPrefix
  protected def akkaAskTimeout = agentConfiguration.akkaAskTimeout
  protected def agentOverview = agentHandle.overview
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

  private[web] def props(gateKeeper: GateKeeper, getCommandHandler: () ⇒ CommandHandler, getAgentActor: () ⇒ AgentHandle, injector: Injector) =
    Props { injector.instance[Factory].apply(gateKeeper, getCommandHandler, getAgentActor) }

  @Singleton
  private class Factory @Inject private(
    sessionRegister: SessionRegister[LoginSession],
    timerService: TimerService,
    extraWebServices: immutable.Seq[ExternalWebService],
    agentConfiguration: AgentConfiguration,
    eventIdGenerator: EventIdGenerator,
    actorSystem: ActorSystem,
    executionContext: ExecutionContext,
    injector: Injector)
  {
    def apply(gateKeeper: GateKeeper, getCommandHandler: () ⇒ CommandHandler, getAgentHandle: () ⇒ AgentHandle) =
      new WebServiceActor(
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
      }
  }
}

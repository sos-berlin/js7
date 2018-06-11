package com.sos.jobscheduler.agent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.CodingDirectives.{decodeRequest, encodeResponse}
import com.google.inject.Injector
import com.sos.jobscheduler.agent.command.CommandHandler
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives
import com.sos.jobscheduler.common.akkahttp.web.auth.CSRF.forbidCSRF
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.{LoginSession, SessionRegister}
import com.sos.jobscheduler.common.time.timer.TimerService
import com.typesafe.config.Config
import javax.inject.{Inject, Singleton}
import monix.execution.Scheduler

/**
 * @author Joacim Zschimmer
 */
private trait RouteProvider
extends ApiRoute
{
  protected def agentOverview = agentHandle.overview
  protected def actorSystem: ActorSystem
  protected def config: Config

  protected final val completeRoute: Route =
    (decodeRequest & encodeResponse) {  // Before handleErrorAndLog to allow simple access to HttpEntity.Strict
      WebLogDirectives(config, actorSystem).handleErrorAndLog() {
        forbidCSRF {
          agentRoute
        }
      }
    }

  private lazy val agentRoute: Route =
    pathSegments("agent/api") {
      apiRoute
    }
}

private[web] object RouteProvider
{
  @Singleton
  final class Factory @Inject private(
    sessionRegister: SessionRegister[LoginSession.Simple],
    timerService: TimerService,
    //taskRegister: TaskRegister,
    agentConfiguration: AgentConfiguration,
    actorSystem: ActorSystem,
    scheduler: Scheduler,
    injector: Injector)
  {
    def toRoute(_gateKeeper: GateKeeper[SimpleUser], getCommandHandler: () ⇒ CommandHandler, getAgentHandle: () ⇒ AgentHandle): Route =
      new RouteProvider {
        protected def gateKeeper = _gateKeeper
        protected def sessionRegister = Factory.this.sessionRegister
        //protected val taskRegister = Factory.this.taskRegister
        protected def commandHandler = getCommandHandler()
        protected def agentHandle = getAgentHandle()
        protected def timerService = Factory.this.timerService
        protected def akkaAskTimeout = agentConfiguration.akkaAskTimeout
        protected def config = agentConfiguration.config
        protected def actorSystem = Factory.this.actorSystem
        protected def scheduler = Factory.this.scheduler
      }.completeRoute
  }
}

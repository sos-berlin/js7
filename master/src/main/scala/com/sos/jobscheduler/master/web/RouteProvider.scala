package com.sos.jobscheduler.master.web

import akka.actor.{ActorRefFactory, ActorSystem}
import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives.handleErrorAndLog
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.master.OrderClient
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.web.simplegui.MasterWebServiceContext
import com.typesafe.config.Config
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
private abstract class RouteProvider(gateKeeper: GateKeeper, injector: Injector)
extends AllRoute {

  private val actorSystem = injector.instance[ActorSystem]
  protected def actorRefFactory = actorSystem
  protected val masterConfiguration       = injector.instance[MasterConfiguration]
  private val config                      = injector.instance[Config]
  protected def eventCollector            = injector.instance[EventCollector]
  protected def eventIdGenerator          = injector.instance[EventIdGenerator]
  implicit protected val executionContext = injector.instance[ExecutionContext]
  protected val webServiceContext = new MasterWebServiceContext(htmlEnabled = true)

  def route(implicit actorRefFactory: ActorRefFactory): Route =
    handleErrorAndLog(config, actorSystem).apply {
      gateKeeper.restrict.apply { _ ⇒
        allRoutes
      }
    }
}

object RouteProvider {
  @Singleton
  final class Factory @Inject private(
    //sessionRegister: SessionRegister[LoginSession],
    timerService: TimerService,
    masterConfiguration: MasterConfiguration,
    eventIdGenerator: EventIdGenerator,
    actorSystem: ActorSystem,
    executionContext: ExecutionContext,
    injector: Injector)
  {
    def toRoute(gateKeeper: GateKeeper, getOrderClient: () ⇒ OrderClient): Route =
      new RouteProvider(gateKeeper, injector) {
        protected val orderClient = getOrderClient()
      }.route(actorSystem)
  }
}

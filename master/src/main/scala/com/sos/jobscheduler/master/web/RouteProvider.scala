package com.sos.jobscheduler.master.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives.handleErrorAndLog
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.master.Master
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.web.simplegui.MasterWebServiceContext
import com.typesafe.config.Config
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
private final class RouteProvider(
  gateKeeper: GateKeeper,
  actorSystem: ActorSystem,
  injector: Injector)
extends AllRoute {

  protected def actorRefFactory = actorSystem
  protected val masterConfiguration       = injector.instance[MasterConfiguration]
  private val config                      = injector.instance[Config]
  protected val orderClient               = injector.instance[Master]
  protected def eventCollector            = injector.instance[EventCollector]
  protected def eventIdGenerator          = injector.instance[EventIdGenerator]
  implicit protected val executionContext = injector.instance[ExecutionContext]
  protected val webServiceContext = new MasterWebServiceContext(htmlEnabled = true)

  def route: Route =
    handleErrorAndLog(config, actorSystem).apply {
      gateKeeper.restrict.apply { _ ⇒
        allRoutes
      }
    }
}

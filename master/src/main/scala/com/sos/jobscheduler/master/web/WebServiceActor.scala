package com.sos.jobscheduler.master.web

import akka.actor.Props
import com.google.inject.Injector
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.sprayutils.WebLogDirectives.handleErrorAndLog
import com.sos.jobscheduler.common.sprayutils.web.auth.GateKeeper
import com.sos.jobscheduler.master.Master
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.web.WebServiceActor._
import com.sos.jobscheduler.master.web.simplegui.MasterWebServiceContext
import com.typesafe.config.Config
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import spray.routing._

/**
  * @author Joacim Zschimmer
  */
private final class WebServiceActor @Inject()(gateKeeper: GateKeeper, injector: Injector)
extends HttpServiceActor with AllRoute {

  protected val masterConfiguration       = injector.instance[MasterConfiguration]
  private val config                      = injector.instance[Config]
  protected val orderClient               = injector.instance[Master]
  protected def eventCollector            = injector.instance[EventCollector]
  protected def eventIdGenerator          = injector.instance[EventIdGenerator]
  implicit protected val executionContext = injector.instance[ExecutionContext]
  protected val webServiceContext = new MasterWebServiceContext(htmlEnabled = true)

  override def postStop() = {
    logger.debug("Stopped")
    super.postStop()
  }

  override def receive = runRoute(
    handleErrorAndLog(config).apply {
      gateKeeper.restrict.apply { _ ⇒
        allRoutes
      }
    })
}

object WebServiceActor {
  private val logger = Logger(getClass)

  def apply(gateKeeper: GateKeeper, injector: Injector) =
    Props { new WebServiceActor(gateKeeper, injector) }
}

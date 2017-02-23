package com.sos.scheduler.engine.master.web

import akka.actor.Props
import com.google.inject.Injector
import com.sos.scheduler.engine.common.guice.GuiceImplicits.RichInjector
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.WebLogDirectives.handleErrorAndLog
import com.sos.scheduler.engine.common.sprayutils.web.auth.GateKeeper
import com.sos.scheduler.engine.master.Master
import com.sos.scheduler.engine.master.configuration.MasterConfiguration
import com.sos.scheduler.engine.master.web.WebServiceActor._
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
  protected val master                    = injector.instance[Master]
  implicit protected val executionContext = injector.instance[ExecutionContext]

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

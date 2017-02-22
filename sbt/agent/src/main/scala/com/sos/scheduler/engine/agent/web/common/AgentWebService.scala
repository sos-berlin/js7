package com.sos.scheduler.engine.agent.web.common

import akka.actor.{ActorRefFactory, ActorSystem}
import com.sos.scheduler.engine.common.sprayutils.WebLogDirectives.handleErrorAndLog
import com.sos.scheduler.engine.common.sprayutils.web.auth.GateKeeper
import com.sos.scheduler.engine.common.sprayutils.web.session.SessionRegister
import com.typesafe.config.Config
import spray.http.Uri.Path
import spray.routing._

/**
 * Standard trait for Agent web services.
 * To be able to mix-in multiple web services, use `addRootRoute` to add a `Route`.
 * Method `route` returns the combined `Route`.
 *
 * @author Joacim Zschimmer
 */
trait AgentWebService {

  protected def uriPathPrefix: String   // Same value for every AgentWebService
  protected def sessionRegister: SessionRegister[LoginSession]
  protected def actorSystem: ActorSystem
  protected def config: Config

  protected final def prefixPath = Path(if (uriPathPrefix.isEmpty) "" else s"/$uriPathPrefix")
  protected final def jobschedulerPath = Path(s"$prefixPath/jobscheduler")
  protected final def agentPath = Path(s"$prefixPath/jobscheduler/agent")
  protected final lazy val routeBuilder = new RouteBuilder(sessionRegister)

  final def buildRoute(gateKeeper: GateKeeper)(implicit actorRefFactory: ActorRefFactory): Route =
    handleErrorAndLog(config, actorSystem).apply {
      routeBuilder.buildRoute(gateKeeper, uriPathPrefix = uriPathPrefix)
    }
}

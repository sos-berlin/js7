package com.sos.jobscheduler.agent.web.common

import akka.actor.{ActorRefFactory, ActorSystem}
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives.handleErrorAndLog
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRegister
import com.typesafe.config.Config

/**
 * Standard trait for Agent web services.
 * To be able to mix-in multiple web services, use `addRootRoute` to add a `Route`.
 * Method `buildRoute` returns the combined `Route`.
 *
 * @author Joacim Zschimmer
 */
trait AgentWebService {

  protected def uriPathPrefix: String   // Same value for every AgentWebService
  protected def sessionRegister: SessionRegister[LoginSession]
  protected def actorSystem: ActorSystem
  protected def config: Config

  protected final def prefixPath = Path(if (uriPathPrefix.isEmpty) "" else s"/$uriPathPrefix")
  protected final def agentPath = Path(s"$prefixPath/agent")
  protected final lazy val routeBuilder = new RouteBuilder(sessionRegister)

  final def buildRoute(gateKeeper: GateKeeper)(implicit actorRefFactory: ActorRefFactory): Route =
    handleErrorAndLog(config, actorSystem).apply {
      routeBuilder.buildRoute(gateKeeper, uriPathPrefix = uriPathPrefix)
    }
}

package com.sos.jobscheduler.master.web

import akka.actor.{ActorRefFactory, ActorSystem}
import akka.http.scaladsl.model.StatusCodes.TemporaryRedirect
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives
import com.sos.jobscheduler.common.akkahttp.html.HtmlDirectives.htmlPreferred
import com.sos.jobscheduler.common.akkahttp.web.auth.CSRF.forbidCSRF
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.master.web.master.MasterRoute
import com.sos.jobscheduler.master.web.serviceprovider.ServiceProviderRoute
import com.typesafe.config.Config

/**
  * @author Joacim Zschimmer
  */
trait CompleteRoute extends ServiceProviderRoute with MasterRoute with WebLogDirectives {

  override protected def gateKeeper: GateKeeper[SimpleUser]
  protected implicit def actorRefFactory: ActorRefFactory
  protected def config: Config
  protected def actorSystem: ActorSystem

  final lazy val completeRoute: Route =
    (decodeRequest & encodeResponse) {  // Before handleErrorAndLog to allow simple access to HttpEntity.Strict
      webLog(userId = None) {
        forbidCSRF {
          route
        }
      }
    }

  private val route =
    pathSegments("master") {
      masterRoute
    } ~
    pathEndOrSingleSlash {
      htmlPreferred {
        redirect("/master", TemporaryRedirect)
      }
    } ~
    serviceProviderRoute  // External service provider's routes, for example Master's own experimantal GUI
}

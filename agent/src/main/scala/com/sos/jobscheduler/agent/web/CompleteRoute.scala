package com.sos.jobscheduler.agent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.CodingDirectives.{decodeRequest, encodeResponse}
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives
import com.sos.jobscheduler.common.akkahttp.web.auth.CSRF.forbidCSRF
import com.typesafe.config.Config

/**
 * @author Joacim Zschimmer
 */
private trait CompleteRoute
extends ApiRoute
{
  protected def actorSystem: ActorSystem
  protected def config: Config

  final lazy val completeRoute: Route =
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

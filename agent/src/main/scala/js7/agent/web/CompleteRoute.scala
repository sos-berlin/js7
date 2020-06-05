package js7.agent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.CodingDirectives.{decodeRequest, encodeResponse}
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.WebLogDirectives
import js7.common.akkahttp.web.auth.CSRF.forbidCSRF
import com.typesafe.config.Config

/**
 * @author Joacim Zschimmer
 */
private[web] trait CompleteRoute
extends WebLogDirectives
with ApiRoute
{
  protected def actorSystem: ActorSystem
  protected def config: Config

  final lazy val completeRoute: Route =
    (decodeRequest & encodeResponse) {  // Before handleErrorAndLog to allow simple access to HttpEntity.Strict
      webLog(userId = None) {
        seal {
          forbidCSRF {
            agentRoute
          }
        }
      }
    }

  private lazy val agentRoute: Route =
    pathSegments("agent/api") {
      apiRoute
    }
}

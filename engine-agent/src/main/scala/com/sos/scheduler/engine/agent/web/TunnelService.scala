package com.sos.scheduler.engine.agent.web

import akka.util.ByteString
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import com.sos.scheduler.engine.tunnel.web.TunnelWebService.tunnelRoute
import scala.concurrent.Future
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait TunnelService extends ServiceStandards {

  //TODO The preconfigured timeout is 20s. http://spray.io/documentation/1.2.3/spray-can/http-server/#request-timeouts

  protected def tunnelRequest(tunnelToken: TunnelToken, requestMessage: ByteString): Future[ByteString]

  private implicit val executionContext = actorRefFactory.dispatcher

  addRoute {
    (post & pathPrefix("agent" / "tunnel" / Segment)) { idString ⇒
      tunnelRoute(TunnelId(idString))(tunnelRequest)
    }
  }
}

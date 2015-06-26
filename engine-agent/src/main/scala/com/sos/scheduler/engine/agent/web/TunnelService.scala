package com.sos.scheduler.engine.agent.web

import akka.util.ByteString
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.common.sprayutils.ByteStreamMarshallers._
import com.sos.scheduler.engine.tunnel.data.Http.PasswordHeaderName
import com.sos.scheduler.engine.tunnel.data.TunnelId
import scala.concurrent.Future
import spray.httpx.unmarshalling._
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait TunnelService extends ServiceStandards {

  //TODO The preconfigured timeout is 20s. http://spray.io/documentation/1.2.3/spray-can/http-server/#request-timeouts

  protected def tunnelRequest(tunnelIdWithPassword: TunnelId.WithPassword, requestMessage: ByteString): Future[ByteString]

  private implicit val executionContext = actorRefFactory.dispatcher

  addRoute {
    (post & pathPrefix("agent" / "tunnel" / Segment)) { tunnelId ⇒
      headerValueByName(PasswordHeaderName) { password ⇒
        entity(as[ByteString]) { request ⇒
          val idWithPassword = TunnelId.WithPassword(TunnelId(tunnelId), TunnelId.Password(password))
          val future = tunnelRequest(idWithPassword, request)
          onSuccess(future) { response: ByteString ⇒ complete(response) }
        }
      }
    }
  }
}

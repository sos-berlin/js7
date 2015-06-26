package com.sos.scheduler.engine.agent.web

import akka.util.ByteString
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.tunnel.TunnelHandler
import com.sos.scheduler.engine.tunnel.data.Http.PasswordHeaderName
import com.sos.scheduler.engine.tunnel.data.TunnelId
import scala.concurrent.ExecutionContext
import spray.http.HttpEntity
import spray.http.MediaTypes._
import spray.httpx.unmarshalling._
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait TunnelService extends ServiceStandards {

  //TODO The preconfigured timeout is 20s. http://spray.io/documentation/1.2.3/spray-can/http-server/#request-timeouts

  protected def tunnelHandler: TunnelHandler
  protected implicit def executionContext: ExecutionContext

  private implicit val ByteStringUnmarshaller = Unmarshaller[ByteString](`application/octet-stream`) {
    case HttpEntity.NonEmpty(contentType, entity) ⇒ entity.toByteString
  }

  addRoute {
    (post & pathPrefix("agent" / "tunnel") & path(Segment)) { tunnelId ⇒
      headerValueByName(PasswordHeaderName) { password ⇒
        entity(as[ByteString]) { request ⇒
          val idWithPassword = TunnelId.WithPassword(TunnelId(tunnelId), TunnelId.Password(password))
          val future = tunnelHandler.request(idWithPassword, request)
          onSuccess(future) { response: ByteString ⇒ complete(response) }
        }
      }
    }
  }
}

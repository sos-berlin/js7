package com.sos.scheduler.engine.tunnel

import akka.util.ByteString
import com.sos.scheduler.engine.common.sprayutils.ByteStreamMarshallers._
import com.sos.scheduler.engine.tunnel.data.Http.SecretHeaderName
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import scala.concurrent.{ExecutionContext, Future}
import spray.httpx.unmarshalling._
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
object TunnelWebService {
  type ExecuteTunneledRequest = (TunnelToken, ByteString) ⇒ Future[ByteString]

//  def tunnelRoute(id: TunnelId)(execute: ExecuteTunnelRequest)(implicit ec: ExecutionContext) =
//    (post & pathPrefix("tunnel" / Segment)) { idString ⇒
//      handleTunnelRequest(TunnelId(idString))(execute)
//    }

  def tunnelRoute(id: TunnelId)(execute: ExecuteTunneledRequest)(implicit ec: ExecutionContext) =
    post {
      headerValueByName(SecretHeaderName) { secret ⇒
        val token = TunnelToken(id, TunnelToken.Secret(secret))
        entity(as[ByteString]) { request ⇒
          val future = execute(token, request)
          onSuccess(future) { response: ByteString ⇒ complete(response) }
        }
      }
    }
}

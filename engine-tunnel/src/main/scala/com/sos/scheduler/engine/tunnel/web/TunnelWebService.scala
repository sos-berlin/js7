package com.sos.scheduler.engine.tunnel.web

import akka.util.ByteString
import com.sos.scheduler.engine.common.sprayutils.ByteStringMarshallers._
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.scheduler.engine.common.utils.IntelliJUtils.intelliJuseImports
import com.sos.scheduler.engine.tunnel.data.Http.SecretHeaderName
import com.sos.scheduler.engine.tunnel.data.{TunnelHandlerOverview, TunnelId, TunnelOverview, TunnelToken}
import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
object TunnelWebService {
  intelliJuseImports(rootFormat _)

  type ExecuteTunneledRequest = (TunnelToken, ByteString) ⇒ Future[ByteString]

  def tunnelRequestRoute(id: TunnelId)(execute: ExecuteTunneledRequest)(implicit ec: ExecutionContext) =
    (pathEndOrSingleSlash & post) {
      headerValueByName(SecretHeaderName) { secret ⇒
        val token = TunnelToken(id, TunnelToken.Secret(secret))
        entity(as[ByteString]) { request ⇒
          val future = execute(token, request)
          onSuccess(future) { response: ByteString ⇒ complete(response) }
        }
      }
    } 

  def tunnelHandlerOverviewRouteComplete(body: ⇒ Future[TunnelHandlerOverview])(implicit ec: ExecutionContext) =
    (pathEndOrSingleSlash & get) {
      onSuccess(body) { response ⇒ complete(response) }
    }

  def tunnelOverviewsRouteComplete(body: ⇒ Future[immutable.Iterable[TunnelOverview]])(implicit ec: ExecutionContext) =
    (pathEndOrSingleSlash & get) {
      onSuccess(body) { response ⇒ complete(response) }
    }
}

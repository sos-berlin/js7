package com.sos.scheduler.engine.tunnel.web

import akka.util.ByteString
import com.sos.scheduler.engine.common.sprayutils.ByteStreamMarshallers._
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.scheduler.engine.common.utils.IntelliJUtils.intelliJuseImports
import com.sos.scheduler.engine.tunnel.data.Http.SecretHeaderName
import com.sos.scheduler.engine.tunnel.data.{TunnelHandlerOverview, TunnelId, TunnelOverview, TunnelToken}
import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import spray.httpx.unmarshalling._
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

  def tunnelHandlerOverviewRoute(f: () ⇒ Future[TunnelHandlerOverview])(implicit ec: ExecutionContext) =
    (pathEndOrSingleSlash & get) {
      onSuccess(f()) { response ⇒ complete(response) }
    }

  def tunnelOverviewsRoute(f: () ⇒ Future[immutable.Iterable[TunnelOverview]])(implicit ec: ExecutionContext) =
    (pathEndOrSingleSlash & get) {
      onSuccess(f()) { response ⇒ complete(response) }
    }
}

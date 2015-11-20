package com.sos.scheduler.engine.tunnel.web

import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.ByteStringMarshallers._
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.scheduler.engine.common.utils.IntelliJUtils.intelliJuseImports
import com.sos.scheduler.engine.tunnel.data.Http.SecretHeaderName
import com.sos.scheduler.engine.tunnel.data.{TunnelHandlerOverview, TunnelId, TunnelOverview, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.{ConnectionClosedException ⇒ TunnelConnectionClosedException}
import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import spray.http.StatusCodes.Gone
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
object TunnelWebServices {
  intelliJuseImports(rootFormat _)

  private val logger = Logger(getClass)

  type ExecuteTunneledRequest = (TunnelToken, ByteString) ⇒ Future[ByteString]

  def tunnelRequestRoute(id: TunnelId)(execute: ExecuteTunneledRequest)(implicit ec: ExecutionContext) =
    (pathEndOrSingleSlash & post) {
      headerValueByName(SecretHeaderName) { secret ⇒
        val token = TunnelToken(id, TunnelToken.Secret(secret))
        entity(as[ByteString]) { request ⇒
          val executed = execute(token, request)
          onSuccess(executed) { response: ByteString ⇒ complete(response) }
          onFailure(executed) {
            case t: TunnelConnectionClosedException ⇒
              logger.error(s"$t")
              complete(Gone)
          }
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

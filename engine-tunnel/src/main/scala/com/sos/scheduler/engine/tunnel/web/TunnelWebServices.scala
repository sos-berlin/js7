package com.sos.scheduler.engine.tunnel.web

import akka.actor.ActorRefFactory
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.ByteStringMarshallers._
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.scheduler.engine.common.utils.IntelliJUtils.intelliJuseImports
import com.sos.scheduler.engine.http.server.heartbeat.HeartbeatService
import com.sos.scheduler.engine.tunnel.data.Http.SecretHeaderName
import com.sos.scheduler.engine.tunnel.data._
import com.sos.scheduler.engine.tunnel.server.{ConnectionClosedException ⇒ TunnelConnectionClosedException, TunnelAccess}
import java.time.Duration
import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import spray.http.StatusCodes.Gone
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._
import spray.routing.ExceptionHandler

/**
 * @author Joacim Zschimmer
 */
object TunnelWebServices {
  intelliJuseImports(rootFormat _)

  type ExecuteTunneledRequest = (TunnelToken, ByteString, Option[Duration]) ⇒ Future[ByteString]
  private val logger = Logger(getClass)

  def tunnelRequestRoute(tunnelId: TunnelId)(
    tunnelAccess: TunnelToken ⇒ TunnelAccess,
    onHeartbeat: (TunnelToken, Duration) ⇒ Unit,
    heartbeatService: HeartbeatService)
    (implicit refFactory: ActorRefFactory) =
  {
    (pathEndOrSingleSlash & post) {
      headerValueByName(SecretHeaderName) { secret ⇒
        val tunnelToken = TunnelToken(tunnelId, TunnelToken.Secret(secret))
        val tunnel = tunnelAccess(tunnelToken)
        heartbeatService.continueHeartbeat(tunnel.idempotence, onClientHeartbeat = onHeartbeat(tunnelToken, _)) ~
        entity(as[ByteString]) { request ⇒
          handleExceptions(connectionClosedExceptionHandler) {
            heartbeatService.startHeartbeat(tunnel.idempotence, onHeartbeat = timeout ⇒ onHeartbeat(tunnelToken, timeout)) {
              timeout ⇒ tunnel.execute(request, timeout)
            }
          }
        }
      }
    }
  }

  private val connectionClosedExceptionHandler = ExceptionHandler {
    case t: TunnelConnectionClosedException ⇒
      logger.error(s"$t")
      complete(Gone)
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

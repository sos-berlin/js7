package com.sos.jobscheduler.tunnel.web

import akka.actor.ActorRefFactory
import akka.util.ByteString
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.sprayutils.ByteStringMarshallers._
import com.sos.jobscheduler.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.common.utils.IntelliJUtils.intelliJuseImports
import com.sos.jobscheduler.tunnel.data.Http.SecretHeaderName
import com.sos.jobscheduler.tunnel.data._
import com.sos.jobscheduler.tunnel.server.{TunnelAccess, TunnelException}
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
    onHeartbeat: (TunnelToken, Duration) ⇒ Unit)
    (implicit refFactory: ActorRefFactory) =
  {
    (pathEndOrSingleSlash & post) {
      headerValueByName(SecretHeaderName) { secret ⇒
        val tunnelToken = TunnelToken(tunnelId, SecretString(secret))
        handleExceptions(tunnelExceptionHandler) {
          val tunnel = tunnelAccess(tunnelToken)
          tunnel.heartbeatService.continueHeartbeat(onClientHeartbeat = onHeartbeat(tunnelToken, _)) ~
            entity(as[ByteString]) { tunnelRequest ⇒
              tunnel.heartbeatService.startHeartbeat(onHeartbeat = timeout ⇒ onHeartbeat(tunnelToken, timeout)) {
                timeout ⇒ tunnel.execute(tunnelRequest, timeout)
              }
            }
        }
      }
    }
  }

  private val tunnelExceptionHandler = ExceptionHandler {
    case t: TunnelException ⇒
      logger.error(s"$t")
      complete(Gone)
  }

  def tunnelHandlerOverviewRouteComplete(body: ⇒ Future[TunnelHandlerOverview])(implicit ec: ExecutionContext) =
    (pathEndOrSingleSlash & get) {
      complete(body)
    }

  def tunnelOverviewsRouteComplete(body: ⇒ Future[immutable.Iterable[TunnelOverview]])(implicit ec: ExecutionContext) =
    (pathEndOrSingleSlash & get) {
      complete(body)
    }
}

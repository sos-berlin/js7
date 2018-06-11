package com.sos.jobscheduler.common.akkahttp.web.auth

import akka.http.scaladsl.model.HttpMethod
import akka.http.scaladsl.model.HttpMethods.{GET, HEAD}
import akka.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import akka.http.scaladsl.model.headers.{HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.server.AuthenticationFailedRejection.CredentialsMissing
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route.seal
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Directive0, Directive1, RejectionHandler, Route}
import com.sos.jobscheduler.base.auth.{HashedPassword, KnownUserPermission, PermissionBundle, User, UserAndPassword, UserId}
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper._
import com.sos.jobscheduler.common.auth.IdToUser
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.typesafe.config.Config
import java.time.Duration
import scala.concurrent._

/**
  * @author Joacim Zschimmer
  */
final class GateKeeper[U <: User](configuraton: Configuration[U], timerService: TimerService, isHttps: Boolean = false)(implicit ec: ExecutionContext) {

  import configuraton.{getIsPublic, httpIsPublic, idToUser, realm}

  private val authenticator = new OurMemoizingAuthenticator(idToUser)
  val credentialsMissing = AuthenticationFailedRejection(CredentialsMissing, HttpChallenges.basic(realm))

  private val credentialRejectionHandler = RejectionHandler.newBuilder()
    .handle {
      case AuthenticationFailedRejection(AuthenticationFailedRejection.CredentialsRejected, challenge) ⇒
        logger.warn(s"HTTP request with invalid authentication rejected - delaying response for ${invalidAuthenticationDelay.pretty}")
        respondWithHeader(`WWW-Authenticate`(challenge)) {
          complete {
            timerService.delayedFuture(invalidAuthenticationDelay, name = "Invalid HTTP authentication") {
              Unauthorized
            }
          }
        }
    }
    .result()

  def authenticateUser(userAndPassword: UserAndPassword): Option[U] =
    authenticator.authenticate(userAndPassword)

  /** Continues with authenticated user or `Anonymous`, or completes with Unauthorized or Forbidden. */
  def authenticate: Directive1[U] =
    new Directive1[U] {
      def tapply(inner: Tuple1[U] ⇒ Route) =
        seal {
          handleRejections(credentialRejectionHandler) {
            authenticateBasic(realm, authenticator).apply { user ⇒
              inner(Tuple1(user))
            }
          }
        }
    }

  /** Continues with authenticated user or `Anonymous`, or completes with Unauthorized or Forbidden. */
  def authorize(user: U, requiredPermissions: PermissionBundle): Directive0 =
    mapInnerRoute(inner ⇒
      seal {
        extractMethod { method ⇒
          if (isAllowed(user, method, requiredPermissions) && user.hasPermissions(requiredPermissions))
            inner
          else if (user.id == UserId.Anonymous)
            reject(credentialsMissing)  // Let a browser show its authentication dialog
          else
            complete(Forbidden)
        }
      })

  /** If KnownUserPermission is not required (meaning Anonymous is allowed)
    * then httpIsPublic and getIsPublic determine the allowance.
    */
  private[auth] def isAllowed(user: U, method: HttpMethod, requiredPermissions: PermissionBundle) =
    user.hasPermissions(KnownUserPermission) ||  // Only Anonymous does not have KnownUserPermission
      !requiredPermissions.contains(KnownUserPermission) && (
        httpIsPublic && !isHttps ||
        getIsPublic && (method == GET || method == HEAD))

  def invalidAuthenticationDelay = configuraton.invalidAuthenticationDelay
}

object GateKeeper {
  private val logger = Logger(getClass)

  final case class Configuration[U <: User](
    /** Basic authentication realm */
    realm: String,
    /** To hamper an attack */
    invalidAuthenticationDelay: Duration,
    /** HTTP is open (assuming local access only) */
    httpIsPublic: Boolean,
    /** HTTP GET is open */
    getIsPublic: Boolean,
    idToUser: UserId ⇒ Option[U])

  object Configuration {
    def fromConfig[U <: User](config: Config, toUser: (UserId, HashedPassword, PermissionBundle) ⇒ U) =
      Configuration[U](
        realm                       = config.getString  ("jobscheduler.webserver.auth.realm"),
        invalidAuthenticationDelay  = config.getDuration("jobscheduler.webserver.auth.invalid-authentication-delay"),
        httpIsPublic                = config.getBoolean ("jobscheduler.webserver.auth.http-is-public"),
        getIsPublic                 = config.getBoolean ("jobscheduler.webserver.auth.get-is-public"),
        idToUser = IdToUser.fromConfig(config, toUser))
  }
}

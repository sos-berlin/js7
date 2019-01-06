package com.sos.jobscheduler.common.akkahttp.web.auth

import akka.http.scaladsl.model.HttpMethod
import akka.http.scaladsl.model.HttpMethods.{GET, HEAD}
import akka.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import akka.http.scaladsl.model.headers.{HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.server.AuthenticationFailedRejection.CredentialsMissing
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route.seal
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Directive0, Directive1, ExceptionHandler, RejectionHandler, Route}
import com.sos.jobscheduler.base.auth.{HashedPassword, PermissionBundle, User, UserAndPassword, UserId, ValidUserPermission}
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper._
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.auth.IdToUser
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.typesafe.config.Config
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class GateKeeper[U <: User](configuraton: Configuration[U],
  isLoopback: Boolean = false, mutual: Boolean = false)
  (implicit scheduler: Scheduler,
    /** For `Route` `seal`. */
    exceptionHandler: ExceptionHandler)
{
  import configuraton.{idToUser, realm}

  private val authenticator = new OurMemoizingAuthenticator(idToUser)
  val credentialsMissing = AuthenticationFailedRejection(CredentialsMissing, HttpChallenges.basic(realm))

  private val credentialRejectionHandler = RejectionHandler.newBuilder()
    .handle {
      case AuthenticationFailedRejection(AuthenticationFailedRejection.CredentialsRejected, challenge) ⇒
        logger.warn(s"HTTP request with invalid authentication rejected - delaying response for ${invalidAuthenticationDelay.pretty}")
        respondWithHeader(`WWW-Authenticate`(challenge)) {
          complete(
            Task.pure(Unauthorized).delayExecution(invalidAuthenticationDelay).runToFuture)
        }

      case AuthenticationFailedRejection(AuthenticationFailedRejection.CredentialsMissing, challenge) ⇒
        // Handling of this case too avoids Akka-streams message "Substream Source cannot be materialized more than once"
        respondWithHeader(`WWW-Authenticate`(challenge)) {
          complete {
            Unauthorized
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
          httpAuthenticate { httpUser ⇒
            inner(Tuple1(httpUser))
          }
        }
    }

  /** Continues with authenticated user or `Anonymous`, or completes with Unauthorized or Forbidden. */
  private def httpAuthenticate: Directive1[U] =
    new Directive1[U] {
      def tapply(inner: Tuple1[U] ⇒ Route) =
        handleRejections(credentialRejectionHandler) {
          authenticateBasic(realm, authenticator).apply { user ⇒
            inner(Tuple1(user))
          }
        }
    }

  /** Continues with authenticated user or `Anonymous`, or completes with Unauthorized or Forbidden. */
  def authorize(user: U, requiredPermissions: PermissionBundle): Directive0 =
    mapInnerRoute(inner ⇒
      seal {
        extractMethod { method ⇒
          if (isAllowed(user, method, requiredPermissions))
            inner
          else if (user.id == UserId.Anonymous)
            reject(credentialsMissing)  // Let a browser show its authentication dialog
          else
            complete(Forbidden)
        }
      })

  /** If ValidUserPermission is not required (meaning Anonymous is allowed)
    * then loopbackIsPublic and getIsPublic determine the allowance.
    */
  private[auth] def isAllowed(user: U, method: HttpMethod, requiredPermissions: PermissionBundle) = {
    import configuraton.{getIsPublic, isPublic, loopbackIsPublic}
    user.hasPermissions(ValidUserPermission) ||  // Only Anonymous does not have ValidUserPermission
      isPublic ||                        // Any access, even POST, is allowed !
      loopbackIsPublic && isLoopback ||  // Any access, even POST, is allowed !
      (method == GET || method == HEAD) &&
        (getIsPublic || !requiredPermissions.contains(ValidUserPermission))
  }

  def invalidAuthenticationDelay = configuraton.invalidAuthenticationDelay

  def boundMessage(binding: WebServerBinding): String =
    s"Bound ${binding.scheme}://${binding.address.getAddress.getHostAddress}:${binding.address.getPort}" +
      (if (binding.mutual) ", client certificate is required" else "") +
      secureStateString

  private def secureStateString: String =
    if (configuraton.isPublic)
      " - ACCESS IS PUBLIC - EVERYONE HAS ACCESS (is-public = true)"
    else if (configuraton.loopbackIsPublic && configuraton.getIsPublic)
      " - ACCESS VIA LOOPBACK (127.*.*.*) INTERFACE OR VIA HTTP METHODS GET OR HEAD IS PUBLIC (loopback-is-public = true, get-is-public = true) "
    else if (configuraton.loopbackIsPublic)
      " - ACCESS OVER LOOPBACK (127.*.*.*) INTERFACE IS PUBLIC (loopback-is-public = true)"
    else if (configuraton.getIsPublic)
      " - ACCESS VIA HTTP METHODS GET OR HEAD IS PUBLIC (get-is-public = true)"
    else
      ""
}

object GateKeeper {
  private val logger = Logger(getClass)

  final case class Configuration[U <: User](
    /** Basic authentication realm */
    realm: String,
    /** To hamper an attack */
    invalidAuthenticationDelay: FiniteDuration,
    /* Anything is allowed for Anonymous */
    isPublic: Boolean = false,
    /** HTTP bound to a loopback interface is allowed for Anonymous */
    loopbackIsPublic: Boolean = false,
    /** HTTP GET is allowed for Anonymous */
    getIsPublic: Boolean = false,
    idToUser: UserId ⇒ Option[U])

  object Configuration {
    def fromConfig[U <: User](config: Config, toUser: (UserId, HashedPassword, PermissionBundle) ⇒ U) =
      Configuration[U](
        realm                       = config.getString  ("jobscheduler.webserver.auth.realm"),
        invalidAuthenticationDelay  = config.getDuration("jobscheduler.webserver.auth.invalid-authentication-delay").toFiniteDuration,
        isPublic                    = config.getBoolean ("jobscheduler.webserver.auth.public"),
        loopbackIsPublic            = config.getBoolean ("jobscheduler.webserver.auth.loopback-is-public"),
        getIsPublic                 = config.getBoolean ("jobscheduler.webserver.auth.get-is-public"),
        idToUser = IdToUser.fromConfig(config, toUser))
  }
}

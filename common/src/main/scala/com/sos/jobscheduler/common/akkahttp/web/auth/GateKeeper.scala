package com.sos.jobscheduler.common.akkahttp.web.auth

import akka.http.scaladsl.model.HttpMethods.{GET, HEAD}
import akka.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import akka.http.scaladsl.model.headers.{HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.model.{HttpMethod, HttpRequest}
import akka.http.scaladsl.server.AuthenticationFailedRejection.CredentialsMissing
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route.seal
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Directive1, ExceptionHandler, RejectionHandler, Route}
import com.sos.jobscheduler.base.auth.{HashedPassword, Permission, User, UserAndPassword, UserId, ValidUserPermission}
import com.sos.jobscheduler.base.utils.ScalazStyle._
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
final class GateKeeper[U <: User](configuraton: Configuration[U], isLoopback: Boolean = false, mutual: Boolean = false)
  (implicit U: User.Companion[U],
    scheduler: Scheduler,
    /** For `Route` `seal`. */
    exceptionHandler: ExceptionHandler)
{
  import configuraton.{idToUser, realm}

  private val authenticator = new OurMemoizingAuthenticator(idToUser)
  private val basicChallenge = HttpChallenges.basic(realm)
  val credentialsMissing = AuthenticationFailedRejection(CredentialsMissing, basicChallenge)
  val wwwAuthenticateHeader = `WWW-Authenticate`(basicChallenge)  // Let's a browser show a authentication dialog

  private val credentialRejectionHandler = RejectionHandler.newBuilder()
    .handle {
      case AuthenticationFailedRejection(AuthenticationFailedRejection.CredentialsRejected, challenge) =>
        logger.warn(s"HTTP request with invalid authentication rejected - delaying response for ${invalidAuthenticationDelay.pretty}")
        respondWithHeader(`WWW-Authenticate`(challenge)) {
          complete(
            Task.pure(Unauthorized).delayExecution(invalidAuthenticationDelay).runToFuture)
        }

      case AuthenticationFailedRejection(AuthenticationFailedRejection.CredentialsMissing, challenge) =>
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
  val authenticate: Directive1[U] =
    new Directive1[U] {
      def tapply(inner: Tuple1[U] => Route) =
        seal {
          httpAuthenticate { httpUser =>
            inner(Tuple1(httpUser))
          }
        }
    }

  /** Continues with authenticated user or `Anonymous`, or completes with Unauthorized or Forbidden. */
  private val httpAuthenticate: Directive1[U] =
    new Directive1[U] {
      def tapply(inner: Tuple1[U] => Route) =
        handleRejections(credentialRejectionHandler) {
          authenticateBasic(realm, authenticator).apply { user =>
            inner(Tuple1(user))
          }
        }
    }

  /** Continues with authenticated user or `Anonymous`, or completes with Unauthorized or Forbidden. */
  def authorize(user: U, requiredPermissions: Set[Permission]): Directive1[U] =
    new Directive1[U] {
      def tapply(inner: Tuple1[U] => Route) =
        seal {
          extractRequest { request =>
            allowedUser(user, request, requiredPermissions) match {
              case Some(authorizedUser) =>
                inner(Tuple1(authorizedUser))
              case None =>
                if (user.isAnonymous)
                  reject(credentialsMissing)  // Let a browser show its authentication dialog
                else
                  complete(Forbidden)
            }
          }
        }
    }

  /** If ValidUserPermission is not required (meaning Anonymous is allowed)
    * then loopbackIsPublic and getIsPublic determine the allowance.
    * In case of loopbackIsPublic or getIsPublic,
    * the returned user gets the `configuration.publicPermissioons`.
    */
  private[auth] def allowedUser(user: U, request: HttpRequest, requiredPermissions: Set[Permission]): Option[U] =
    ifPublic(request.method) match {
      case Some(reason) =>
        if (!user.isAnonymous) logger.warn(s"User '${user.id.string}' has logged in despite $reason")
        val empoweredUser = U.addPermissions(user, reason match {
          case IsPublic | LoopbackIsPublic => configuraton.publicPermissions
          case GetIsPublic                 => configuraton.publicGetPermissions
        })
        if (empoweredUser.grantedPermissions != user.grantedPermissions) {
          logger.debug(s"Granting user '${empoweredUser.id.string}' all rights for ${request.method.value} ${request.uri.path} due to $reason")
        }
        Some(empoweredUser)

      case None =>
        isPermitted(user, requiredPermissions, request.method) ? user
    }

  private[auth] def ifPublic(method: HttpMethod): Option[AuthorizationReason] = {
    import configuraton.{getIsPublic, isPublic, loopbackIsPublic}
    if (isPublic)
      Some(IsPublic)
    else if (isLoopback && loopbackIsPublic)
      Some(LoopbackIsPublic)
    else if (getIsPublic && isGet(method))
      Some(GetIsPublic)
    else
      None
  }

  private def isPermitted(user: U, requiredPermissions: Set[Permission], method: HttpMethod): Boolean =
    user.hasPermissions(requiredPermissions) && (
      requiredPermissions.contains(ValidUserPermission) ||  // If ValidUserPermission is not required (Anonymous is allowed)...
      user.hasPermission(ValidUserPermission) ||            // ... then allow Anonymous ... (only Anonymous does not have ValidUserPermission)
      isGet(method))                                        // ... only to read

  def invalidAuthenticationDelay = configuraton.invalidAuthenticationDelay

  def boundMessage(binding: WebServerBinding): String =
    s"Bound ${binding.scheme}://${binding.address.getAddress.getHostAddress}:${binding.address.getPort}" +
      (if (binding.mutual) ", client certificate is required" else "") +
      secureStateString

  private def secureStateString: String =
    if (configuraton.isPublic)
      " - ACCESS IS PUBLIC - EVERYONE HAS ACCESS (public = true)"
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

  private def isGet(method: HttpMethod) = method == GET || method == HEAD

  final case class Configuration[U <: User](
    /** Basic authentication realm */
    realm: String,
    /** To hamper an attack */
    invalidAuthenticationDelay: FiniteDuration,
    publicPermissions: Set[Permission] = Set.empty,
    publicGetPermissions: Set[Permission] = Set.empty,
    /* Anything is allowed for Anonymous */
    isPublic: Boolean = false,
    /** HTTP bound to a loopback interface is allowed for Anonymous */
    loopbackIsPublic: Boolean = false,
    /** HTTP GET is allowed for Anonymous */
    getIsPublic: Boolean = false,
    idToUser: UserId => Option[U])

  object Configuration {
    def fromConfig[U <: User](
      config: Config,
      toUser: (UserId, HashedPassword, Set[Permission]) => U,
      toPermission: PartialFunction[String, Permission] = PartialFunction.empty)
    =
      Configuration[U](
        realm                       = config.getString  ("jobscheduler.webserver.auth.realm"),
        invalidAuthenticationDelay  = config.getDuration("jobscheduler.webserver.auth.invalid-authentication-delay").toFiniteDuration,
        isPublic                    = config.getBoolean ("jobscheduler.webserver.auth.public"),
        loopbackIsPublic            = config.getBoolean ("jobscheduler.webserver.auth.loopback-is-public"),
        getIsPublic                 = config.getBoolean ("jobscheduler.webserver.auth.get-is-public"),
        idToUser = IdToUser.fromConfig(config, toUser, toPermission))
  }

  private[auth] sealed trait AuthorizationReason
  private[auth] object IsPublic extends AuthorizationReason {
    override def toString = "public=true"
  }
  private[auth] object LoopbackIsPublic extends AuthorizationReason {
    override def toString = "loopback-is-public=true"
  }
  private[auth] object GetIsPublic extends AuthorizationReason {
    override def toString = "get-is-public=true"
  }
}

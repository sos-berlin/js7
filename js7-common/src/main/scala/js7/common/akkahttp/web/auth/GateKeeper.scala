package js7.common.akkahttp.web.auth

import akka.http.scaladsl.model.HttpMethods.{GET, HEAD}
import akka.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import akka.http.scaladsl.model.headers.{HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.model.{HttpMethod, HttpRequest}
import akka.http.scaladsl.server.AuthenticationFailedRejection.CredentialsMissing
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route.seal
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Directive1, ExceptionHandler, RejectionHandler, Route}
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.auth.{GetPermission, HashedPassword, Permission, SimpleUser, SuperPermission, User, UserAndPassword, UserId, ValidUserPermission}
import js7.base.time.ScalaTime._
import js7.base.utils.ScalazStyle._
import js7.common.akkahttp.web.auth.GateKeeper._
import js7.common.auth.IdToUser
import js7.common.scalautil.Logger
import js7.common.time.JavaTimeConverters._
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
  import configuraton.{getIsPublic, idToUser, isPublic, loopbackIsPublic, realm}

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

  def authenticateUser(userAndPassword: UserAndPassword): Option[U] = {
    if (!userAndPassword.userId.isAnonymous) ifPublic foreach { reason =>
      logger.warn(s"User '${userAndPassword.userId.string}' logs in with credentials despite $reason")
    }
    authenticator.authenticate(userAndPassword)
  }

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
        //??? if (!user.isAnonymous) logger.warn(s"User '${user.id.string}' has logged in despite $reason")
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

  private[auth] def ifPublic(method: HttpMethod): Option[AuthorizationReason] =
    ifPublic orElse ((getIsPublic && isGet(method)) ? GetIsPublic)

  private[auth] def ifPublic: Option[AuthorizationReason] =
    if (isPublic)
      Some(IsPublic)
    else if (isLoopback && loopbackIsPublic)
      Some(LoopbackIsPublic)
    else
      None

  private def isPermitted(user: U, requiredPermissions: Set[Permission], method: HttpMethod): Boolean =
    user.hasPermissions(requiredPermissions) && (
      requiredPermissions.contains(ValidUserPermission) ||  // If ValidUserPermission is not required (Anonymous is allowed)...
      user.hasPermission(ValidUserPermission) ||            // ... then allow Anonymous ... (only unempowered Anonymous does not have ValidUserPermission)
      isGet(method))                                        // ... only to read

  def invalidAuthenticationDelay = configuraton.invalidAuthenticationDelay

  def secureStateString: String =
    if (configuraton.isPublic)
      " - ACCESS IS PUBLIC - EVERYONE HAS ACCESS (public = true)"
    else if (configuraton.loopbackIsPublic && configuraton.getIsPublic)
      " - ACCESS VIA LOOPBACK (127.*.*.*) INTERFACE OR VIA HTTP METHODS GET OR HEAD IS PUBLIC (loopback-is-public = true, get-is-public = true) "
    else if (configuraton.loopbackIsPublic)
      " - ACCESS VIA LOOPBACK (127.*.*.*) INTERFACE IS PUBLIC (loopback-is-public = true)"
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
    /* Anything is allowed for Anonymous */
    isPublic: Boolean = false,
    /** HTTP bound to a loopback interface is allowed for Anonymous */
    loopbackIsPublic: Boolean = false,
    /** HTTP GET is allowed for Anonymous */
    getIsPublic: Boolean = false,
    idToUser: UserId => Option[U])
  {
    val publicPermissions = Set[Permission](SuperPermission)
    val publicGetPermissions = Set[Permission](GetPermission)
  }

  object Configuration {
    def fromConfig[U <: User](
      config: Config,
      toUser: (UserId, HashedPassword, Set[Permission]) => U,
      toPermission: PartialFunction[String, Permission] = PartialFunction.empty)
    =
      Configuration[U](
        realm                       = config.getString  ("js7.web.server.auth.realm"),
        invalidAuthenticationDelay  = config.getDuration("js7.web.server.auth.invalid-authentication-delay").toFiniteDuration,
        isPublic                    = config.getBoolean ("js7.web.server.auth.public"),
        loopbackIsPublic            = config.getBoolean ("js7.web.server.auth.loopback-is-public"),
        getIsPublic                 = config.getBoolean ("js7.web.server.auth.get-is-public"),
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

  def forTest(isPublic: Boolean = false, config: Config = ConfigFactory.empty)(implicit eh: ExceptionHandler, s: Scheduler)
  : GateKeeper[SimpleUser] =
    new GateKeeper(
      GateKeeper.Configuration.fromConfig(
        config.withFallback(ConfigFactory.parseString(
         s"""js7.web.server.auth {
            |  realm = "TEST REALM"
            |  invalid-authentication-delay = 100ms
            |  loopback-is-public = false
            |  get-is-public = false
            |  public = $isPublic
            |}
            |""".stripMargin)),
        SimpleUser.apply))
}

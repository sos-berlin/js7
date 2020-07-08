package js7.common.akkahttp.web.auth

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.HttpMethods.{GET, HEAD}
import akka.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import akka.http.scaladsl.model.headers.{HttpChallenges, `Tls-Session-Info`, `WWW-Authenticate`}
import akka.http.scaladsl.model.{HttpMethod, HttpRequest}
import akka.http.scaladsl.server.AuthenticationFailedRejection.CredentialsMissing
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route.seal
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Directive1, ExceptionHandler, RejectionHandler, Route}
import com.typesafe.config.{Config, ConfigFactory}
import java.security.cert.X509Certificate
import js7.base.auth.{DistinguishedName, GetPermission, HashedPassword, Permission, SimpleUser, SuperPermission, User, UserAndPassword, UserId, ValidUserPermission}
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkahttp.StandardMarshallers._
import js7.common.akkahttp.web.auth.GateKeeper._
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.auth.IdToUser
import js7.common.scalautil.Logger
import js7.common.time.JavaTimeConverters._
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class GateKeeper[U <: User](scheme: WebServerBinding.Scheme, configuration: Configuration[U], isLoopback: Boolean = false)
  (implicit U: User.Companion[U],
    scheduler: Scheduler,
    /** For `Route` `seal`. */
    exceptionHandler: ExceptionHandler)
{
  // https://tools.ietf.org/html/rfc7235#section-3.1: "A server generating a 401 (Unauthorized) response
  // MUST send a WWW-Authenticate header field containing at least one challenge."

  import configuration.{getIsPublic, idToUser, isPublic, loopbackIsPublic, realm}

  private val htttpClientAuthRequired = scheme == WebServerBinding.Https && configuration.httpsClientAuthRequired
  private val authenticator = new OurMemoizingAuthenticator(idToUser)
  private val basicChallenge = HttpChallenges.basic(realm)
  val credentialsMissing = AuthenticationFailedRejection(CredentialsMissing, basicChallenge)
  val wwwAuthenticateHeader = `WWW-Authenticate`(basicChallenge)  // Let's a browser show a authentication dialog

  private val credentialRejectionHandler = RejectionHandler.newBuilder()
    .handle {
      case AuthenticationFailedRejection(AuthenticationFailedRejection.CredentialsRejected, challenge) =>
        logger.warn(s"HTTP request with invalid authentication rejected - delaying response for ${invalidAuthenticationDelay.pretty}")
        respondWithHeader(`WWW-Authenticate`(challenge)) {
          completeDelayed(
            Unauthorized)
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

  /** Continues with authenticated user or `Anonymous`, or completes with Unauthorized. */
  val authenticate: Directive1[U] =
    new Directive1[U] {
      def tapply(inner: Tuple1[U] => Route) =
        seal {
          httpAuthenticate { httpUser =>
            if (!htttpClientAuthRequired)
              inner(Tuple1(httpUser))
            else clientHttpsAuthenticate { maybeHttpsUser =>
              ((httpUser.id, maybeHttpsUser) match {
                case (_, None) => Right(httpUser)
                case (h, Some(t)) if h == t.id => Right(httpUser)
                case (UserId.Anonymous, Some(httpsUser)) => Right(httpsUser)
                case _ => Left(Problem.pure("HTTP logged-in user does not match client's HTTPS certificate user"))
              }) match {
                case Left(problem) => complete(Unauthorized -> problem)
                case Right(user) => inner(Tuple1(user))
              }
            }
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

  private def clientHttpsAuthenticate: Directive1[Option[U]] =
    new Directive1[Option[U]] {
      def tapply(inner: Tuple1[Option[U]] => Route) =
        optionalHeaderValueByType[`Tls-Session-Info`](()) {
          case None =>
            if (htttpClientAuthRequired)
              complete(Unauthorized -> Problem.pure("A client HTTPS certificate is required"))
            else
              inner(Tuple1(None))

          case Some(tlsSessionInfo) =>
            val checkedUser = tlsSessionInfo.peerCertificates match {
              case (cert: X509Certificate) :: Nil =>
                certToUser(cert).map(Some.apply)

              case certs =>
                // Safari sends the CA certificate with the client certicate. Why this? Due to generate-certificate ???
                certs.collect {
                  case o: X509Certificate if Option(o.getBasicConstraints).forall(_ == -1) => o
                } match {
                  case cert :: Nil =>
                    logger.debug("HTTPS client has sent the client and a CA certificate. We ignore the latter")
                    certToUser(cert).map(Some.apply)
                  case _ =>
                    if (certs.nonEmpty) logger.debug(s"HTTPS client certificates rejected: ${certs.mkString(", ")}")
                    Left(certs.length match {
                      case n if n > 1 => Problem.pure(s"One and only one peer certificate is required (not $n)")
                      case _ => Problem.pure("A client X.509 certificate is required")
                    })
                }
              }
            checkedUser match {
              case Left(problem) => completeDelayed(Unauthorized -> problem)
              case Right(user) => inner(Tuple1(user))
            }
        }
    }

  private def certToUser(cert: X509Certificate): Checked[U] =
    DistinguishedName.checked(cert.getSubjectX500Principal.getName)
      .flatMap(dn =>
        configuration.distinguishedNameToUser(dn)
          .toChecked(Problem.pure(s"Unknown distinguished name '$dn'")))

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
          case IsPublic | LoopbackIsPublic => configuration.publicPermissions
          case GetIsPublic                 => configuration.publicGetPermissions
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

  private def completeDelayed(body: => ToResponseMarshallable): Route =
    complete(Task(body).delayExecution(invalidAuthenticationDelay).runToFuture)

  def invalidAuthenticationDelay = configuration.invalidAuthenticationDelay

  def secureStateString: String =
    if (configuration.isPublic)
      " - ACCESS IS PUBLIC - EVERYONE HAS ACCESS (public = true)"
    else if (configuration.loopbackIsPublic && configuration.getIsPublic)
      " - ACCESS VIA LOOPBACK (127.*.*.*) INTERFACE OR VIA HTTP METHODS GET OR HEAD IS PUBLIC (loopback-is-public = true, get-is-public = true) "
    else if (configuration.loopbackIsPublic)
      " - ACCESS VIA LOOPBACK (127.*.*.*) INTERFACE IS PUBLIC (loopback-is-public = true)"
    else if (configuration.getIsPublic)
      " - ACCESS VIA HTTP METHODS GET OR HEAD IS PUBLIC (get-is-public = true)"
    else if (htttpClientAuthRequired)
      " - HTTPS client authentication (mutual TLS) required"
    else
      ""
}

object GateKeeper
{
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
    httpsClientAuthRequired: Boolean = false,
    idToUser: UserId => Option[U],
    distinguishedNameToUser: DistinguishedName => Option[U])
  {
    val publicPermissions = Set[Permission](SuperPermission)
    val publicGetPermissions = Set[Permission](GetPermission)
  }

  object Configuration {
    def fromConfig[U <: User](
      config: Config,
      toUser: (UserId, HashedPassword, Set[Permission], Seq[DistinguishedName]) => U,
      toPermission: PartialFunction[String, Permission] = PartialFunction.empty)
    = {
      val idToUser = IdToUser.fromConfig(config, toUser, toPermission)
      Configuration[U](
        realm                       = config.getString  ("js7.web.server.auth.realm"),
        invalidAuthenticationDelay  = config.getDuration("js7.web.server.auth.invalid-authentication-delay").toFiniteDuration,
        isPublic                    = config.getBoolean ("js7.web.server.auth.public"),
        loopbackIsPublic            = config.getBoolean ("js7.web.server.auth.loopback-is-public"),
        getIsPublic                 = config.getBoolean ("js7.web.server.auth.get-is-public"),
        httpsClientAuthRequired     = config.getBoolean ("js7.web.server.auth.https-client-authentication"),
        idToUser = idToUser,
        distinguishedNameToUser = idToUser.distinguishedNameToUser)
    }
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

  def forTest(scheme: WebServerBinding.Scheme = WebServerBinding.Http, isPublic: Boolean = false,
    config: Config = ConfigFactory.empty)(implicit eh: ExceptionHandler, s: Scheduler)
  : GateKeeper[SimpleUser] =
    new GateKeeper(
      scheme = scheme,
      GateKeeper.Configuration.fromConfig(
        config.withFallback(ConfigFactory.parseString(
         s"""js7.web.server.auth {
            |  realm = "TEST REALM"
            |  invalid-authentication-delay = 100ms
            |  https-client-authentication = on
            |  loopback-is-public = false
            |  get-is-public = false
            |  public = $isPublic
            |}
            |""".stripMargin)),
        SimpleUser.apply))
}

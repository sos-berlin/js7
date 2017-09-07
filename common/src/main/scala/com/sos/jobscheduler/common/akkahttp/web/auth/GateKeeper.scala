package com.sos.jobscheduler.common.akkahttp.web.auth

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Directive0, Directive1, ExceptionHandler, RejectionHandler, Route}
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils._
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper._
import com.sos.jobscheduler.common.auth.User.Anonymous
import com.sos.jobscheduler.common.auth.{HashedPassword, User, UserId}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.typesafe.config.Config
import java.time.Duration
import scala.concurrent._

/**
  * @author Joacim Zschimmer
  */
final class GateKeeper(configuraton: Configuration, csrf: CSRF, timerService: TimerService, isUnsecuredHttp: Boolean = false)(implicit ec: ExecutionContext) {

  import configuraton.{getIsPublic, httpIsPublic, invalidAuthenticationDelay, realm, userIdToHashedPassword}

  def restrict: Directive1[User] =
    new Directive1[User] {
      def tapply(inner: Tuple1[User] ⇒ Route) =
        extractSettings { routingSettings ⇒
          retrictRelaxed {
            if (isUnsecuredHttp && httpIsPublic)
              inner(Tuple1(Anonymous))
            else
              handleRejections(failIfCredentialsRejected(invalidAuthenticationDelay, timerService)) {
                val authenticator = new OurAuthenticator(userIdToHashedPassword())
                authenticateBasic(realm = realm, authenticator) { user: User ⇒
                  handleRejections(RejectionHandler.default) {
                    handleExceptions(ExceptionHandler.default(routingSettings)) {
                      inner(Tuple1(user))
                    }
                  }
                }
              } ~
              passIf(getIsPublic) {
                (get | head) {
                  inner(Tuple1(Anonymous))
                }
              }
          }
        }
    }

  val retrictRelaxed: Directive0 =
    csrf.rejectSomeCSRF
}

object GateKeeper {
  private val logger = Logger(getClass)

  def forTest()(implicit ec: ExecutionContext) = new GateKeeper(
    Configuration(
      realm = "TEST-REALM",
      httpIsPublic = true,
      userIdToHashedPassword = () ⇒ PartialFunction.empty),
    new CSRF(CSRF.Configuration.ForTest),
    TimerService(idleTimeout = Some(1.s)),
    isUnsecuredHttp = true)

  final case class Configuration(
    /** Basic authentication realm */
    realm: String,
    /** To hamper an attack */
    invalidAuthenticationDelay: Duration = 1.s,
    /** HTTP is open (assuming local access only) */
    httpIsPublic: Boolean = false,
    /** HTTP GET is open */
    getIsPublic: Boolean = false,
    userIdToHashedPassword: () ⇒ UserId ⇒ Option[HashedPassword] = () ⇒ _ ⇒ None)
    //provideAccessTokenValidator: () ⇒ PartialFunction[SecretString, UserId] = () ⇒ PartialFunction.empty)

  object Configuration {
    def fromSubConfig(authConfig: Config) =
      new Configuration(
        realm = authConfig.getString("realm"),
        invalidAuthenticationDelay = authConfig.getDuration("invalid-authentication-delay"),
        httpIsPublic = authConfig.getBoolean("http-is-public"),
        getIsPublic = authConfig.getBoolean("get-is-public"))
  }

  def failIfCredentialsRejected(delay: Duration, timerService: TimerService)(implicit ec: ExecutionContext): RejectionHandler =
    RejectionHandler.newBuilder()
    .handle {
      case AuthenticationFailedRejection(AuthenticationFailedRejection.CredentialsRejected, _) ⇒
        complete {
          logger.warn(s"HTTP request with invalid authentication rejected - delaying response for ${delay.pretty}")
          timerService.delayedFuture(delay, name = "Invalid HTTP authentication") {
            StatusCodes.Unauthorized
          }
        }
      }
    .result()
}

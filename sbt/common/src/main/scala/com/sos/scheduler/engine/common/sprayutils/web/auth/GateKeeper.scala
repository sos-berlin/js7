package com.sos.scheduler.engine.common.sprayutils.web.auth

import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.{User, UserAndPassword, UserId}
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.SprayUtils._
import com.sos.scheduler.engine.common.sprayutils.web.auth.GateKeeper._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.typesafe.config.Config
import java.time.Duration
import scala.concurrent._
import spray.routing.AuthenticationFailedRejection.CredentialsRejected
import spray.routing.Directives._
import spray.routing._
import spray.routing.authentication._
import spray.util.LoggingContext

/**
  * @author Joacim Zschimmer
  */
final class GateKeeper(configuraton: Configuration, csrf: CSRF, isUnsecuredHttp: Boolean = false)(implicit ec: ExecutionContext) {

  import configuraton.{getIsPublic, httpIsPublic, invalidAuthenticationDelay, provideAccessTokenValidator, providePasswordValidator, realm}

  def restrict(implicit routingSettings: RoutingSettings, log: LoggingContext): Directive0 =
    mapInnerRoute { inner ⇒
      retrictRelaxed {
        if (isUnsecuredHttp && httpIsPublic)
          inner
        else
          handleRejections(failIfCredentialsRejected(invalidAuthenticationDelay)) {
            val authenticator = new SimpleUserPassAuthenticator(providePasswordValidator(), provideAccessTokenValidator())
            authenticate(BasicAuth(authenticator, realm = realm)) { _: User ⇒
              handleRejections(RejectionHandler.Default) {
                handleExceptions(ExceptionHandler.default) {
                  inner
                }
              }
            }
          } ~
          passIf(getIsPublic) {
            (get | head) {
              inner
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
      providePasswordValidator = () ⇒ _ ⇒ false),
    new CSRF(CSRF.Configuration.Default),
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
    providePasswordValidator: () ⇒ UserAndPassword ⇒ Boolean = () ⇒ _ ⇒ false,
    provideAccessTokenValidator: () ⇒ PartialFunction[SecretString, UserId] = () ⇒ PartialFunction.empty)

  object Configuration {
    def fromSubConfig(authConfig: Config) =
      new Configuration(
        realm = authConfig.getString("realm"),
        invalidAuthenticationDelay = authConfig.getDuration("invalid-authentication-delay"),
        httpIsPublic = authConfig.getBoolean("http-is-public"),
        getIsPublic = authConfig.getBoolean("get-is-public"))
  }

  def failIfCredentialsRejected(delay: Duration)(implicit ec: ExecutionContext) = RejectionHandler {
    case rejections @ AuthenticationFailedRejection(CredentialsRejected, headers) :: _  ⇒
      detach(()) {
        logger.warn(s"HTTP request with invalid authentication rejected")
        blocking {
          sleep(delay)
        }
        RejectionHandler.Default(rejections)
      }
  }
}

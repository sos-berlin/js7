package com.sos.scheduler.engine.common.sprayutils.web.auth

import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.{EncodedPasswordValidator, UserAndPassword}
import com.sos.scheduler.engine.common.configutils.Configs.ConvertibleConfig
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

/**
  * @author Joacim Zschimmer
  */
final class GateKeeper(configuraton: Configuration, isUnsecuredHttp: Boolean = false)(implicit ec: ExecutionContext) {

  import configuraton.{getIsPublic, httpIsPublic, invalidAuthenticationDelay, providePasswordValidator, realm}

  val allows: Directive0 =
    mapInnerRoute { inner ⇒
      if (isUnsecuredHttp && httpIsPublic)
        inner
      else
        handleRejections(failIfCredentialsRejected(invalidAuthenticationDelay)) {
          authenticate(BasicAuth(new SimpleUserPassAuthenticator(providePasswordValidator()), realm = realm)) { _ ⇒
            inner
          }
        } ~
        passIf(getIsPublic) {  // After authenticate, to return its error
          (get | head) {
            inner
          }
        }
    }
}

object GateKeeper {
  private val logger = Logger(getClass)

  def forTest()(implicit ec: ExecutionContext) = new GateKeeper(
    Configuration(
      realm = "TEST-REALM",
      providePasswordValidator = () ⇒ _ ⇒ false,
      httpIsPublic = true),
    isUnsecuredHttp = true)

  final case class Configuration(
    /** Basic authentication realm */
    realm: String,
    providePasswordValidator: () ⇒ UserAndPassword ⇒ Boolean,
    /** To hamper an attack */
    invalidAuthenticationDelay: Duration = 1.s,
    /** HTTP is open (assuming local access only) */
    httpIsPublic: Boolean = false,
    /** HTTP GET is open */
    getIsPublic: Boolean = false)

  object Configuration {
    def fromSubConfig(config: Config) = new Configuration(
      realm = config.getString("realm"),
      providePasswordValidator = () ⇒ new EncodedPasswordValidator(config.getConfig("users").optionAs[SecretString]),  // Configuration "users" is only required with authentication switched on
      invalidAuthenticationDelay = config.getDuration("invalid-authentication-delay"),
      httpIsPublic = config.getBoolean("http-is-public"),
      getIsPublic = config.getBoolean("get-is-public")
    )
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

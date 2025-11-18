package js7.controller.client

import cats.effect.{IO, ResourceIO}
import com.typesafe.config.Config
import js7.base.auth.UserAndPassword
import js7.base.configutils.Configs.*
import js7.base.io.https.{HttpsConfig, KeyStoreRef, TrustStoreRef}
import js7.base.service.Service
import js7.base.session.SessionApi
import js7.base.web.Uri
import js7.common.configuration.BasicConfiguration
import js7.common.http.{PekkoHttpClient, TextApi}
import js7.common.pekkoutils.Pekkos
import js7.data.session.HttpSessionApi
import org.apache.pekko.actor.ActorSystem

/**
  * @author Joacim Zschimmer
  */
private[controller] final class PekkoHttpControllerTextApi(
  protected val baseUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val print: String => Unit,
  conf : BasicConfiguration)
  (using protected val actorSystem: ActorSystem)
extends
  TextApi, HttpSessionApi, PekkoHttpClient, SessionApi.HasUserAndPassword,
  Service.TrivialReleasable:

  protected val config: Config = config"pekko.log-dead-letters = 0".withFallback(conf.config)

  protected val name = "PekkoHttpControllerTextApi"

  protected def uriPrefixPath = "/controller"

  private val controllerUris = ControllerUris(baseUri)

  protected def httpClient = this

  protected def sessionUri = controllerUris.session

  protected def serverName = "JS7 Controller"

  protected def commandUri = controllerUris.command

  protected def apiUri(tail: String) = controllerUris.api(tail)

  protected lazy val httpsConfig =
    HttpsConfig(
      keyStoreRef = None,
      trustStoreRefs =
        conf.maybeConfigDirectory.flatMap: dir =>
          KeyStoreRef.clientFromConfig(config, configDirectory = dir).toOption
        .map(TrustStoreRef.fromKeyStore)
        .toSeq)

  def release =
    IO:
      logOpenSession()
      super.close()


object PekkoHttpControllerTextApi:

  def resource(
    controllerUri: Uri,
    userAndPassword: Option[UserAndPassword],
    print: String => Unit,
    conf: BasicConfiguration)
  : ResourceIO[PekkoHttpControllerTextApi] =
    for
      given ActorSystem <- Pekkos.actorSystemResource(name = "PekkoHttpControllerTextApi")
      result <- Service:
        new PekkoHttpControllerTextApi(controllerUri, userAndPassword, print, conf)
    yield
      result

package js7.controller.client

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.base.auth.UserAndPassword
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.{HttpsConfig, KeyStoreRef, TrustStoreRef}
import js7.base.session.SessionApi
import js7.base.utils.HasCloser
import js7.base.web.Uri
import js7.common.akkautils.ProvideActorSystem
import js7.common.http.{AkkaHttpClient, TextApi}
import js7.controller.client.AkkaHttpControllerTextApi.*
import js7.data.session.HttpSessionApi

/**
  * @author Joacim Zschimmer
  */
private[controller] final class AkkaHttpControllerTextApi(
  protected val baseUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val print: String => Unit,
  configDirectory: Option[Path] = None)
extends HasCloser with ProvideActorSystem with TextApi with HttpSessionApi with AkkaHttpClient
with SessionApi.HasUserAndPassword:

  protected val config = config"akka.log-dead-letters = 0"

  protected val name = "AkkaHttpControllerTextApi"

  protected def uriPrefixPath = "/controller"

  private val controllerUris = ControllerUris(Uri(s"$baseUri/controller"))

  protected def httpClient = this

  protected def sessionUri = controllerUris.session

  protected def serverName = "JS7 Controller"

  protected def commandUri = controllerUris.command

  protected def apiUri(tail: String) = controllerUris.api(tail)

  protected lazy val httpsConfig =
    HttpsConfig(
      keyStoreRef = None,
      trustStoreRefs = configDirectory
        .flatMap(dir =>
          KeyStoreRef.clientFromConfig(configDirectoryToConfig(dir), configDirectory = dir)
            .toOption)
        .map(TrustStoreRef.fromKeyStore)
        .toSeq)

  closer.onClose { super.close() }

  override def close() =
    logOpenSession()
    closer.close()

object AkkaHttpControllerTextApi:
  // Like ControllerConfiguration.configDirectoryToConfig
  private def configDirectoryToConfig(configDirectory: Path): Config =
    ConfigFactory
      .empty
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "controller.conf", secret = false))

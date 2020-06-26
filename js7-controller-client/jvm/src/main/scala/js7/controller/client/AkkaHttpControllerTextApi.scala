package js7.controller.client

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.base.auth.UserAndPassword
import js7.base.session.HttpSessionApi
import js7.base.utils.HasCloser
import js7.base.web.Uri
import js7.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import js7.common.akkautils.ProvideActorSystem
import js7.common.configutils.Configs.parseConfigIfExists
import js7.common.http.{AkkaHttpClient, TextApi}
import js7.common.scalautil.FileUtils.syntax._
import js7.controller.client.AkkaHttpControllerTextApi._

/**
  * @author Joacim Zschimmer
  */
private[controller] final class AkkaHttpControllerTextApi(
  protected val baseUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val print: String => Unit,
  configDirectory: Option[Path] = None)
extends HasCloser with ProvideActorSystem with TextApi with HttpSessionApi with AkkaHttpClient
{
  protected val config = ConfigFactory.empty

  protected val name = "AkkaHttpControllerTextApi"

  protected def uriPrefixPath = "/controller"

  private val controllerUris = ControllerUris(Uri(s"$baseUri/controller"))

  protected def httpClient = this

  protected def sessionUri = controllerUris.session

  protected def serverName = "JS7 Controller"

  protected def commandUri = controllerUris.command

  protected def apiUri(tail: String) = controllerUris.api(tail)

  protected def keyStoreRef = None

  protected lazy val trustStoreRefs = configDirectory
    .flatMap(dir =>
      KeyStoreRef.fromConfig(configDirectoryConfig(dir), dir / "private/https-keystore.p12")
        .toOption)
    .map(TrustStoreRef.fromKeyStore)
    .toSeq

  closer.onClose { super.close() }

  override def close() = closer.close()
}

object AkkaHttpControllerTextApi
{
  // Like ControllerConfiguration.configDirectoryConfig
  private def configDirectoryConfig(configDirectory: Path): Config =
    ConfigFactory
      .empty
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "controller.conf", secret = false))
}

package js7.master.client

import js7.base.auth.UserAndPassword
import js7.base.session.HttpSessionApi
import js7.base.utils.HasCloser
import js7.base.web.Uri
import js7.common.akkahttp.https.TrustStoreRef
import js7.common.akkautils.ProvideActorSystem
import js7.common.configutils.Configs.parseConfigIfExists
import js7.common.http.{AkkaHttpClient, TextApi}
import js7.common.scalautil.FileUtils.syntax._
import js7.master.client.AkkaHttpMasterTextApi._
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[master] final class AkkaHttpMasterTextApi(
  protected val baseUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val print: String => Unit,
  configDirectory: Option[Path] = None)
extends HasCloser with ProvideActorSystem with TextApi with HttpSessionApi with AkkaHttpClient
{
  protected val config = ConfigFactory.empty

  protected val name = "AkkaHttpMasterTextApi"

  protected def uriPrefixPath = "/master"

  private val masterUris = MasterUris(Uri(s"$baseUri/master"))

  protected def httpClient = this

  protected def sessionUri = masterUris.session

  protected def serverName = "JS7 Master"

  protected def commandUri = masterUris.command

  protected def apiUri(tail: String) = masterUris.api(tail)

  protected def keyStoreRef = None

  protected lazy val trustStoreRef = configDirectory.flatMap(configDir =>
    TrustStoreRef.fromConfig(configDirectoryConfig(configDir), default = configDir / "private/https-keystore.p12")
      .toOption)

  closer.onClose { super.close() }

  override def close() = closer.close()
}

object AkkaHttpMasterTextApi
{
  // Like MasterConfiguration.configDirectoryConfig
  private def configDirectoryConfig(configDirectory: Path): Config =
    ConfigFactory
      .empty
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "master.conf", secret = false))
}

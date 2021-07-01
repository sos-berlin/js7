package js7.agent.client

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.agent.client.AkkaHttpAgentTextApi._
import js7.agent.data.web.AgentUris
import js7.base.auth.UserAndPassword
import js7.base.configutils.Configs._
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.https.{KeyStoreRef, TrustStoreRef}
import js7.base.utils.HasCloser
import js7.base.web.Uri
import js7.common.akkautils.ProvideActorSystem
import js7.common.http.{AkkaHttpClient, TextApi}
import js7.data.session.HttpSessionApi

/**
  * @author Joacim Zschimmer
  */
private[agent] final class AkkaHttpAgentTextApi(
  agentUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val print: String => Unit,
  configDirectory: Option[Path] = None)
extends HasCloser with ProvideActorSystem with TextApi with HttpSessionApi with AkkaHttpClient
{
  protected val name = "AkkaHttpAgentTextApi"
  protected val config = config"akka.log-dead-letters = 0"

  private val agentUris = AgentUris(agentUri)

  protected def keyStoreRef = None

  protected lazy val trustStoreRefs = configDirectory.toList.flatMap { configDir =>
    // Use Controller's keystore as truststore for client access, using also Controller's store-password
    val controllersConfig = configDirectoryConfig(configDir)
    KeyStoreRef.fromConfig(controllersConfig, configDir / "private/https-keystore.p12")
      .map(TrustStoreRef.fromKeyStore)
      .toOption
  }

  protected val baseUri = agentUri

  protected def uriPrefixPath = "/agent"

  protected def serverName = "JS7 Agent"

  protected val sessionUri = agentUris.session

  protected val commandUri = agentUris.command

  protected def httpClient = this

  protected def apiUri(tail: String) = agentUris.api(tail)

  closer.onClose { super.close() }

  override def close() = closer.close()
}

object AkkaHttpAgentTextApi
{
  // Like AgentConfiguration.configDirectoryConfig
  private def configDirectoryConfig(configDirectory: Path): Config =
    ConfigFactory
      .empty
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "controller.conf", secret = false))
}

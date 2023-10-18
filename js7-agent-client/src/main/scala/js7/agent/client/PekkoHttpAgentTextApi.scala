package js7.agent.client

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.agent.client.PekkoHttpAgentTextApi.*
import js7.agent.data.web.AgentUris
import js7.base.auth.UserAndPassword
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.{HttpsConfig, KeyStoreRef, TrustStoreRef}
import js7.base.session.SessionApi
import js7.base.utils.HasCloser
import js7.base.web.Uri
import js7.common.http.{PekkoHttpClient, TextApi}
import js7.common.pekkoutils.ProvideActorSystem
import js7.data.session.HttpSessionApi

/**
  * @author Joacim Zschimmer
  */
private[agent] final class PekkoHttpAgentTextApi(
  agentUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val print: String => Unit,
  configDirectory: Option[Path] = None)
extends HasCloser with ProvideActorSystem with TextApi with HttpSessionApi with PekkoHttpClient
with SessionApi.HasUserAndPassword:

  protected val name = "PekkoHttpAgentTextApi"
  protected val config = config"pekko.log-dead-letters = 0"

  private val agentUris = AgentUris(agentUri)

  protected def keyStoreRef = None

  protected lazy val httpsConfig =
    HttpsConfig(
      keyStoreRef = None,
      trustStoreRefs = configDirectory.toList
        .flatMap { configDir =>
          // Use Controller's keystore as truststore for client access, using also Controller's store-password
          val controllersConfig = configDirectoryToConfig(configDir)
          KeyStoreRef.clientFromConfig(controllersConfig, configDirectory = configDir)
            .map(TrustStoreRef.fromKeyStore)
            .toOption
        })

  protected val baseUri = agentUri

  protected def uriPrefixPath = "/agent"

  protected def serverName = "JS7 Agent"

  protected val sessionUri = agentUris.session

  protected val commandUri = agentUris.command

  protected def httpClient = this

  protected def apiUri(tail: String) = agentUris.api(tail)

  closer.onClose { super.close() }

  override def close() =
    logOpenSession()
    closer.close()


object PekkoHttpAgentTextApi:
  // Like AgentConfiguration.configDirectoryToConfig
  private def configDirectoryToConfig(configDirectory: Path): Config =
    ConfigFactory
      .empty
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "controller.conf", secret = false))

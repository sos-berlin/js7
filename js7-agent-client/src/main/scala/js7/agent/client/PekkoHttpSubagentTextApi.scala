package js7.agent.client

import cats.effect.unsafe.IORuntime
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.Json
import java.nio.file.Path
import js7.agent.client.PekkoHttpSubagentTextApi.*
import js7.agent.data.commands.AgentCommand
import js7.agent.data.web.{AgentUris, SubagentUris}
import js7.base.auth.UserAndPassword
import js7.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.{HttpsConfig, KeyStoreRef, TrustStoreRef}
import js7.base.problem.Checked.Ops
import js7.base.session.SessionApi
import js7.base.utils.HasCloser
import js7.base.web.Uri
import js7.common.http.{PekkoHttpClient, TextApi}
import js7.common.pekkoutils.ProvideActorSystem
import js7.data.session.HttpSessionApi
import js7.data.subagent.SubagentCommand
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
private[agent] final class PekkoHttpSubagentTextApi(
  agentUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val print: String => Unit,
  configDirectory: Option[Path] = None)
  (using protected val executionContext: ExecutionContext)
extends HasCloser,
  ProvideActorSystem, TextApi, HttpSessionApi, PekkoHttpClient, SessionApi.HasUserAndPassword:

  protected val name = "PekkoHttpSubagentTextApi"
  protected val config = config"pekko.log-dead-letters = 0"

  private val subagentUris = SubagentUris(agentUri)

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

  protected def uriPrefixPath = "/subagent"

  protected def serverName = "JS7 Agent"

  protected val sessionUri = subagentUris.session

  protected val commandUri = subagentUris.command

  private[client] val directorUris = AgentUris(subagentUris.subagentUri)

  protected def httpClient = this

  protected def apiUri(tail: String) = subagentUris.api(tail)

  closer.onClose { super.close() }

  override def close(): Unit =
    logOpenSession()
    closer.close()

  /** Executes SubagentCommand and AgentCommand. */
  override def executeCommand(command: String)(using IORuntime): Unit =
    val cmdJson = command.parseJson.orThrow
    cmdJson.as[SubagentCommand] match
      case Right(_) => super.executeCommand(command)
      case Left(_) =>
        cmdJson.as[AgentCommand] match
          case Right(_) =>
            val response = awaitResult:
              httpClient.post[Json, Json](uri = directorUris.command, cmdJson)
            printer.doPrint(response.compactPrint)
          case Left(_) => super.executeCommand(command)

object PekkoHttpSubagentTextApi:
  // Like AgentConfiguration.configDirectoryToConfig
  private def configDirectoryToConfig(configDirectory: Path): Config =
    ConfigFactory
      .empty
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "controller.conf", secret = false))

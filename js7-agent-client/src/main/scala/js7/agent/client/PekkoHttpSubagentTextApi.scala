package js7.agent.client

import cats.effect.kernel.Resource
import cats.effect.{IO, ResourceIO}
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.Json
import java.nio.file.Path
import js7.agent.client.PekkoHttpSubagentTextApi.*
import js7.agent.data.commands.AgentCommand
import js7.agent.data.web.{AgentUris, SubagentUris}
import js7.base.auth.UserAndPassword
import js7.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import js7.base.config.Js7Config
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.{HttpsConfig, KeyStoreRef, TrustStoreRef}
import js7.base.problem.Checked.Ops
import js7.base.session.SessionApi
import js7.base.web.Uri
import js7.common.configuration.BasicConfiguration
import js7.common.http.{PekkoHttpClient, TextApi}
import js7.common.pekkoutils.Pekkos
import js7.data.session.HttpSessionApi
import js7.data.subagent.SubagentCommand
import org.apache.pekko.actor.ActorSystem

/**
  * @author Joacim Zschimmer
  */
private[agent] final class PekkoHttpSubagentTextApi private(
  agentUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val print: String => Unit,
  conf: BasicConfiguration)
  (using protected val actorSystem: ActorSystem)
extends
  TextApi, HttpSessionApi, PekkoHttpClient, SessionApi.HasUserAndPassword:

  protected val name = "PekkoHttpSubagentTextApi"
  protected val config = config"pekko.log-dead-letters = 0"

  private val subagentUris = SubagentUris(agentUri)

  protected def keyStoreRef = None

  protected lazy val httpsConfig =
    HttpsConfig(
      keyStoreRef = None,
      trustStoreRefs = conf.maybeConfigDirectory.toList
        .flatMap: configDir =>
          // Use Controller's keystore as truststore for client access, using also Controller's store-password
          val controllersConfig = configDirectoryToConfig(configDir)
          KeyStoreRef.clientFromConfig(controllersConfig, configDirectory = configDir)
            .map(TrustStoreRef.fromKeyStore)
            .toOption)

  protected val baseUri = agentUri

  protected def uriPrefixPath = "/subagent"

  protected def serverName = "JS7 Agent"

  protected val sessionUri = subagentUris.session

  protected val commandUri = subagentUris.command

  private[client] val directorUris = AgentUris(subagentUris.subagentUri)

  protected def httpClient = this

  protected def apiUri(tail: String) = subagentUris.api(tail)

  override def close(): Unit =
    logOpenSession()
    super.close()

  /** Executes SubagentCommand and AgentCommand. */
  override def executeCommand(command: String): IO[Unit] =
    IO.defer:
      val cmdJson = command.parseJson.orThrow
      cmdJson.as[SubagentCommand] match
        case Right(_) => super.executeCommand(command)
        case Left(_) =>
          cmdJson.as[AgentCommand] match
            case Right(_) =>
              httpClient.post[Json, Json](uri = directorUris.command, cmdJson).flatMap: response =>
                IO(printer.doPrint(response.compactPrint))
            case Left(_) => super.executeCommand(command)


object PekkoHttpSubagentTextApi:

  def resource(
    agentUri: Uri,
    userAndPassword: Option[UserAndPassword],
    print: String => Unit,
    conf: BasicConfiguration)
  : ResourceIO[PekkoHttpSubagentTextApi] =
    for
      given ActorSystem <- Pekkos.actorSystemResource(name = "PekkoHttpControllerTextApi")
      result <- Resource.fromAutoCloseable:
        IO(new PekkoHttpSubagentTextApi(agentUri, userAndPassword, print, conf))
    yield
      result


  // Like AgentConfiguration.configDirectoryToConfig
  private def configDirectoryToConfig(configDirectory: Path): Config =
    ConfigFactory.systemProperties
      .withFallback(parseConfigIfExists(configDirectory / "private" / "private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "agent.conf", secret = false))
      .withFallback(Js7Config.defaultConfig)

package com.sos.jobscheduler.agent.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.agent.client.AkkaHttpAgentTextApi._
import com.sos.jobscheduler.agent.data.web.AgentUris
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.common.akkahttp.https.{Https, KeyStoreRef}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.configutils.Configs.parseConfigIfExists
import com.sos.jobscheduler.common.http.{AkkaHttpClient, TextApi}
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.data.agent.AgentAddress
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[agent] final class AkkaHttpAgentTextApi(
  agentUri: AgentAddress,
  protected val print: String ⇒ Unit,
  configDirectory: Option[Path] = None)
extends HasCloser with ProvideActorSystem with TextApi with SessionApi with AkkaHttpClient {

  private val agentUris = AgentUris(agentUri)

  protected override lazy val httpsConnectionContextOption =
    for (configDir ← configDirectory) yield
      Https.loadHttpsConnectionContext(
        KeyStoreRef.fromConfig(configDirectoryConfig(configDir), default = configDir / "private/https-keystore.p12").orThrow)

  protected def baseUri = Uri(agentUri.string)

  protected def uriPrefixPath = "/agent"

  protected def serverName = "JobScheduler Agent"

  protected val sessionUri = agentUris.session.toString

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
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf"))
      .withFallback(parseConfigIfExists(configDirectory / "master.conf"))
}

package com.sos.jobscheduler.agent.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.agent.client.AkkaHttpAgentTextApi._
import com.sos.jobscheduler.agent.data.web.AgentUris
import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.common.akkahttp.https.{AkkaHttps, TrustStoreRef}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.configutils.Configs.{ConvertibleConfig, parseConfigIfExists}
import com.sos.jobscheduler.common.http.{AkkaHttpClient, TextApi}
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.data.agent.AgentAddress
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
private[agent] final class AkkaHttpAgentTextApi(
  agentUri: AgentAddress,
  protected val print: String ⇒ Unit,
  configDirectory: Option[Path] = None)
extends HasCloser with ProvideActorSystem with TextApi with SessionApi with AkkaHttpClient {

  private val agentUris = AgentUris(agentUri)

  protected override lazy val httpsConnectionContextOption = {
    configDirectory.flatMap { configDir ⇒
      // Use Master's keystore as truststore for client access, using also Master's store-password
      val mastersConfig = configDirectoryConfig(configDir)
      mastersConfig.optionAs[String]("jobscheduler.https.keystore.store-password").flatMap { storePassword ⇒
        val file = mastersConfig.optionAs[Path]("jobscheduler.https.keystore.file") getOrElse configDir / "private/https-keystore.p12"
        val config = ConfigFactory.parseMap(Map("jobscheduler.https.truststore.store-password" → storePassword).asJava)
        TrustStoreRef.fromConfig(config, default = file).onProblem(o ⇒ logger.debug(s"No keystore: $o"))
      }
      .map(trustStoreRef ⇒
        AkkaHttps.loadHttpsConnectionContext(trustStoreRef = Some(trustStoreRef)))
    }
  }

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
  private val logger = Logger(getClass)

  // Like AgentConfiguration.configDirectoryConfig
  private def configDirectoryConfig(configDirectory: Path): Config =
    ConfigFactory
      .empty
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf"))
      .withFallback(parseConfigIfExists(configDirectory / "master.conf"))
}

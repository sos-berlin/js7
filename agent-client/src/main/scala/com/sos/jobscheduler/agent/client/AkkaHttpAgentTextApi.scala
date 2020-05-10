package com.sos.jobscheduler.agent.client

import com.sos.jobscheduler.agent.client.AkkaHttpAgentTextApi._
import com.sos.jobscheduler.agent.data.web.AgentUris
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.session.HttpSessionApi
import com.sos.jobscheduler.base.utils.HasCloser
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkahttp.https.TrustStoreRef
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.configutils.Configs.{ConvertibleConfig, parseConfigIfExists}
import com.sos.jobscheduler.common.http.{AkkaHttpClient, TextApi}
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.Logger
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import scala.jdk.CollectionConverters._

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
  protected val config = ConfigFactory.empty

  private val agentUris = AgentUris(agentUri)

  protected def keyStoreRef = None

  protected lazy val trustStoreRef = configDirectory.flatMap { configDir =>
    // Use Master's keystore as truststore for client access, using also Master's store-password
    val mastersConfig = configDirectoryConfig(configDir)
    mastersConfig.optionAs[String]("jobscheduler.https.keystore.store-password").flatMap { storePassword =>
      val file = mastersConfig.optionAs[Path]("jobscheduler.https.keystore.file") getOrElse configDir / "private/https-keystore.p12"
      val config = ConfigFactory.parseMap(Map("jobscheduler.https.truststore.store-password" -> storePassword).asJava)
      TrustStoreRef.fromConfig(config, default = file).onProblem(o => logger.debug(s"No keystore: $o"))
    }
  }

  protected val baseUri = agentUri

  protected def uriPrefixPath = "/agent"

  protected def serverName = "JobScheduler Agent Server"

  protected val sessionUri = agentUris.session

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
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "master.conf", secret = false))
}

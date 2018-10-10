package com.sos.jobscheduler.master.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.common.akkahttp.https.{AkkaHttps, TrustStoreRef}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.configutils.Configs.parseConfigIfExists
import com.sos.jobscheduler.common.http.{AkkaHttpClient, TextApi}
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.master.client.AkkaHttpMasterTextApi._
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[master] final class AkkaHttpMasterTextApi(
  protected val baseUri: Uri,
  protected val print: String ⇒ Unit,
  configDirectory: Option[Path] = None)
extends HasCloser with ProvideActorSystem with TextApi with SessionApi with AkkaHttpClient {

  protected def uriPrefixPath = "/master"

  private val masterUris = MasterUris(s"$baseUri/master")

  protected def httpClient = this

  protected def sessionUri = masterUris.session

  protected def serverName = "JobScheduler Master"

  protected def commandUri = masterUris.command

  protected def apiUri(tail: String) = masterUris.api(tail)

  protected override lazy val httpsConnectionContextOption =
    for {
      configDir ← configDirectory
      trustStoreRef ← TrustStoreRef.fromConfig(configDirectoryConfig(configDir), default = configDir / "private/https-keystore.p12").toOption
    } yield
      AkkaHttps.loadHttpsConnectionContext(trustStoreRef = Some(trustStoreRef))

  closer.onClose { super.close() }

  override def close() = closer.close()
}

object AkkaHttpMasterTextApi
{
  // Like MasterConfiguration.configDirectoryConfig
  private def configDirectoryConfig(configDirectory: Path): Config =
    ConfigFactory
      .empty
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf"))
      .withFallback(parseConfigIfExists(configDirectory / "master.conf"))
}

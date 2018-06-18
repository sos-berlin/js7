package com.sos.jobscheduler.agent.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.agent.data.web.AgentUris
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.https.{Https, KeystoreReference}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.http.{AkkaHttpClient, TextClient}
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.data.agent.AgentAddress
import java.nio.file.Files.exists
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[agent] final class TextAgentClient(
  agentUri: AgentAddress,
  protected val print: String ⇒ Unit,
  configDirectory: Option[Path] = None)
extends HasCloser with AkkaHttpClient with ProvideActorSystem with TextClient {

  private val agentUris = AgentUris(agentUri)

  private lazy val keystoreReferenceOption =
    for {
      dir ← configDirectory
      file = dir / "public-https.jks" if exists(file)
    } yield
      KeystoreReference(
        file.toURI.toURL,
        storePassword = Some(SecretString("jobscheduler")),
        keyPassword = Some(SecretString("jobscheduler")))

  protected def baseUri = Uri(agentUri.string)
  protected def uriPrefixPath = "/agent"

  protected def serverName = "JobScheduler Agent"

  protected val sessionUri = agentUris.session.toString

  protected val commandUri = agentUris.command

  protected def httpClient = this

  protected def apiUri(tail: String) = agentUris.api(tail)

  protected override val httpsConnectionContext =
    keystoreReferenceOption.fold(super.httpsConnectionContext)(Https.toHttpsConnectionContext)

  closer.onClose { super.close() }

  override def close() = closer.close()
}

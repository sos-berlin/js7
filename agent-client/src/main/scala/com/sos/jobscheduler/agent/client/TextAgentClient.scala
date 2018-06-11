package com.sos.jobscheduler.agent.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.agent.data.web.AgentUris
import com.sos.jobscheduler.common.akkahttp.https.{Https, KeystoreReference}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.http.{AkkaHttpClient, TextClient}
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.data.agent.AgentAddress

/**
  * @author Joacim Zschimmer
  */
private[agent] final class TextAgentClient(
  agentUri: AgentAddress,
  protected val print: String ⇒ Unit,
  keystore: Option[KeystoreReference] = None)
extends HasCloser with AkkaHttpClient with ProvideActorSystem with TextClient {

  private val agentUris = AgentUris(agentUri)

  protected def baseUri = Uri(agentUri.string)
  protected def uriPrefixPath = "/agent"

  protected def serverName = "JobScheduler Agent"

  protected val sessionUri = agentUris.session.toString

  protected val commandUri = agentUris.command

  protected def httpClient = this

  protected def apiUri(tail: String) = agentUris.api(tail)

  protected override val httpsConnectionContext = keystore/*Option*/ map Https.toHttpsConnectionContext getOrElse super.httpsConnectionContext

  closer.onClose { super.close() }

  override def close() = closer.close()
}

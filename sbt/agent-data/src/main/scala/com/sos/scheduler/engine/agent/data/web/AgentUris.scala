package com.sos.scheduler.engine.agent.data.web

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.web.AgentUris._
import com.sos.scheduler.engine.data.agent.AgentAddress
import com.sos.scheduler.engine.tunnel.data.TunnelId
import spray.http.Uri
import spray.http.Uri.Path

/**
 * URIs of the JobScheduler Agent.
 *
 * @author Joacim Zschimmer
 */
final class AgentUris private(agentUri: AgentAddress) {

  val prefixedUri = Uri(s"$agentUri/$AgentUriConstantPrefix")

  def overview = uriString(Api)

  val command = uriString(s"$Api/command")

  def fileExists(filePath: String): String =
    (withPath("api/fileExists") withQuery ("file" → filePath)).toString()

  object task {
    def overview = uriString(s"$Api/task")

    def tasks = uriString(s"$Api/task/")

    def apply(id: AgentTaskId) = uriString(Path(s"$Api/task") / id.string)
  }

  object tunnel {
    def overview = uriString(s"$Api/tunnel")

    def tunnels = uriString(s"$Api/tunnel/")

    def apply(id: TunnelId) = uriString(Path(s"$Api/tunnel") / id.string)
  }

  def apply(relativeUri: String) = uriString(Path(stripLeadingSlash(relativeUri)))

  def api(relativeUri: String) = uriString(s"$Api/${stripLeadingSlash(relativeUri)}")

  private def uriString(path: Path): String = uriString(Uri(path = path))

  private def uriString(uri: Uri): String = withPath(uri).toString()

  def withPath(uri: Uri) = {
    val u = uri.resolvedAgainst(prefixedUri)
    u.copy(path = Path(s"${prefixedUri.path}/${stripLeadingSlash(uri.path.toString())}"))
  }

  override def toString = agentUri.string
}

object AgentUris {
  private val AgentUriConstantPrefix = "jobscheduler/agent"
  private val Api = "api"
  val LicenseKeyHeaderName = "X-JobScheduler-LicenseKey"

  def apply(address: AgentAddress): AgentUris = new AgentUris(address)

  def apply(agentUri: String) = new AgentUris(AgentAddress.normalized(agentUri))

  private def stripLeadingSlash(o: String) =
    o match {
      case _ ⇒ o stripPrefix "/"
    }
}

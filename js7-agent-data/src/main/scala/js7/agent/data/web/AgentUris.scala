package js7.agent.data.web

import akka.http.scaladsl.model.Uri.{Path, Query}
import akka.http.scaladsl.model.{Uri => AkkaUri}
import js7.agent.data.web.AgentUris._
import js7.base.web.Uri
import js7.data.event.{Event, EventRequest}

/**
 * URIs of the JS7 Agent.
 *
 * @author Joacim Zschimmer
 */
final class AgentUris private(agentUri: Uri)
{
  private val prefixedUri = AkkaUri(s"$agentUri/agent")

  val overview = toUri("api")
  val session = toUri("api/session")
  val command = toUri("api/command")

  def controllersEvents[E <: Event](request: EventRequest[E]) =
    toUri("api/event", Query(request.toQueryParameters: _*))

  def apply(relativeUri: String) = toUri(stripLeadingSlash(relativeUri))

  def api(relativeUri: String) =
    if (relativeUri.isEmpty)
      toUri("api")
    else
      toUri(s"api/${stripLeadingSlash(relativeUri)}")

  private def toUri(uri: String, query: Query): Uri = Uri(withPath(uri).withQuery(query).toString)

  private def toUri(uri: String): Uri = Uri(withPath(uri).toString)

  private def withPath(uri: AkkaUri): AkkaUri = {
    val u = uri.resolvedAgainst(prefixedUri)
    u.copy(path = Path(s"${prefixedUri.path}/${stripLeadingSlash(uri.path.toString())}"))
  }

  override def toString = agentUri.toString
}

object AgentUris
{
  def apply(uri: Uri) = new AgentUris(Uri(uri.string stripSuffix "/"))

  private def stripLeadingSlash(o: String) = o stripPrefix "/"
}

package js7.agent.data.web

import js7.agent.data.web.AgentUris.*
import js7.base.web.Uri
import js7.base.web.Uris.encodeQuery
import js7.data.event.{Event, EventRequest}

/**
 * URIs of the JS7 Agent.
 *
 * @author Joacim Zschimmer
 */
final class AgentUris private(agentUri: Uri):

  private val prefixedUri = Uri(s"$agentUri/agent")

  val overview = toUri("api")
  val session = toUri("api/session")
  val command = toUri("api/command")

  def controllersEvents[E <: Event](request: EventRequest[E]) =
    toUri("api/event" + encodeQuery(request.toQueryParameters*))

  def apply(relativeUri: String) = toUri(stripLeadingSlash(relativeUri))

  def api(relativeUri: String) =
    if relativeUri.isEmpty then
      toUri("api")
    else
      toUri(s"api/${stripLeadingSlash(relativeUri)}")

  private def toUri(uri: String): Uri =
    prefixedUri / uri

  override def toString = agentUri.toString

object AgentUris:
  def apply(uri: Uri) = new AgentUris(Uri(uri.string stripSuffix "/"))

  private def stripLeadingSlash(o: String) = o stripPrefix "/"

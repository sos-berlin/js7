package js7.agent.data.web

import js7.agent.data.web.AgentUris.*
import js7.base.time.ScalaTime.RichFiniteDuration
import js7.base.web.Uri
import js7.base.web.Uris.encodeQuery
import js7.data.event.{Event, EventRequest}
import scala.concurrent.duration.FiniteDuration

/**
 * URIs of the JS7 Agent.
 *
 * @author Joacim Zschimmer
 */
final class AgentUris private(val agentUri: Uri):

  private val prefixedUri = Uri(s"$agentUri/agent")

  lazy val subagentUris: SubagentUris = SubagentUris(agentUri)
  val overview: Uri = toUri("api")
  val session: Uri = toUri("api/session")
  val command: Uri = toUri("api/command")

  def controllersEvents[E <: Event](
    request: EventRequest[E],
    heartbeat: Option[FiniteDuration] = None)
  : Uri =
    toUri("api/event" +
      encodeQuery:
        request.toQueryParameters ++
          (heartbeat.fold(Nil)(h => ("heartbeat" -> h.toDecimalString) :: Nil)))

  def apply(relativeUri: String): Uri =
    toUri(stripLeadingSlash(relativeUri))

  def api(relativeUri: String): Uri =
    if relativeUri.isEmpty then
      toUri("api")
    else
      toUri(s"api/${stripLeadingSlash(relativeUri)}")

  private def toUri(uri: String): Uri =
    prefixedUri / uri

  override def toString = agentUri.toString


object AgentUris:
  def apply(uri: Uri): AgentUris =
    new AgentUris(Uri(uri.string stripSuffix "/"))

  private def stripLeadingSlash(o: String) =
    o stripPrefix "/"

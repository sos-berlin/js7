package com.sos.jobscheduler.agent.data.web

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.{Path, Query}
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.web.AgentUris._
import com.sos.jobscheduler.data.agent.AgentAddress
import com.sos.jobscheduler.data.event.{Event, EventRequest}
import com.sos.jobscheduler.data.order.OrderId

/**
 * URIs of the JobScheduler Agent.
 *
 * @author Joacim Zschimmer
 */
final class AgentUris private(agentUri: AgentAddress) {

  val prefixedUri = Uri(s"$agentUri/agent")

  val overview = toUri("api")
  val session = toUri("api/session")
  val command = toUri("api/command")

  object task {
    val overview = toUri("api/task")
    val tasks = toUri("api/task/")

    def apply(id: AgentTaskId) = toUri(Path("api/task") / id.string)
  }

  object order {
    def apply(orderId: OrderId): Uri =
      toUri(Uri(path = Path("api/order") / orderId.string))

    val ids: Uri =
      toUri(Uri(path = Path("api/order/")) withQuery Query("return" → "OrderId"))

    val events: Uri =
      toUri(Uri(path = Path("api/order/")) withQuery Query("return" → "OrderEvent"))

    val orders: Uri =
      toUri(Uri(path = Path("api/order/")) withQuery Query("return" → "Order"))
  }

  def mastersEvents[E <: Event](request: EventRequest[E]) =
    toUri(Uri(path = Uri.Path("api/master/event")) withQuery Query(request.toQueryParameters: _*))

  def apply(relativeUri: String) = toUri(Path(stripLeadingSlash(relativeUri)))

  def api(relativeUri: String) =
    if (relativeUri.isEmpty)
      toUri("api")
    else
      toUri(s"api/${stripLeadingSlash(relativeUri)}")

  private def toUri(path: Path): Uri = toUri(Uri(path = path))

  private def toUri(uri: Uri): Uri = withPath(uri)

  def withPath(uri: Uri) = {
    val u = uri.resolvedAgainst(prefixedUri)
    u.copy(path = Path(s"${prefixedUri.path}/${stripLeadingSlash(uri.path.toString())}"))
  }

  override def toString = agentUri.string
}

object AgentUris {
  val LicenseKeyHeaderName = "X-JobScheduler-LicenseKey"

  def apply(address: AgentAddress): AgentUris = new AgentUris(address)

  def apply(agentUri: String) = new AgentUris(AgentAddress.normalized(agentUri))

  private def stripLeadingSlash(o: String) =
    o match {
      case _ ⇒ o stripPrefix "/"
    }
}

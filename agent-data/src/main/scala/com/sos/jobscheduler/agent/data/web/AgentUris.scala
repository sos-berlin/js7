package com.sos.jobscheduler.agent.data.web

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.{Path, Query}
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.web.AgentUris._
import com.sos.jobscheduler.data.agent.AgentAddress
import com.sos.jobscheduler.data.event.EventRequest
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}

/**
 * URIs of the JobScheduler Agent.
 *
 * @author Joacim Zschimmer
 */
final class AgentUris private(agentUri: AgentAddress) {

  val prefixedUri = Uri(s"$agentUri/$AgentUriConstantPrefix")

  def overview = toUri(Api)

  val command = toUri(s"$Api/command")

  object task {
    def overview = toUri(s"$Api/task")

    def tasks = toUri(s"$Api/task/")

    def apply(id: AgentTaskId) = toUri(Path(s"$Api/task") / id.string)
  }

  object order {
    def apply(orderId: OrderId): Uri =
      toUri(Uri(path = Path(s"$Api/order") / orderId.string))

    def ids: Uri =
      toUri(Uri(path = Path(s"$Api/order/")) withQuery Query("return" → "OrderId"))

    def events: Uri =
      toUri(Uri(path = Path(s"$Api/order/")) withQuery Query("return" → "OrderEvent"))

    def orders: Uri =
      toUri(Uri(path = Path(s"$Api/order/")) withQuery Query("return" → "Order"))
  }

  def mastersEvents(request: EventRequest[OrderEvent]) =
    toUri(Uri(path = Uri.Path(s"$Api/master/event")) withQuery Query(request.toQueryParameters: _*))

  def apply(relativeUri: String) = toUri(Path(stripLeadingSlash(relativeUri)))

  def api(relativeUri: String) = toUri(s"$Api/${stripLeadingSlash(relativeUri)}")

  private def toUri(path: Path): Uri = toUri(Uri(path = path))

  private def toUri(uri: Uri): Uri = withPath(uri)

  def withPath(uri: Uri) = {
    val u = uri.resolvedAgainst(prefixedUri)
    u.copy(path = Path(s"${prefixedUri.path}/${stripLeadingSlash(uri.path.toString())}"))
  }

  override def toString = agentUri.string
}

object AgentUris {
  private val AgentUriConstantPrefix = "agent"
  private val Api = "api"
  val LicenseKeyHeaderName = "X-JobScheduler-LicenseKey"

  def apply(address: AgentAddress): AgentUris = new AgentUris(address)

  def apply(agentUri: String) = new AgentUris(AgentAddress.normalized(agentUri))

  private def stripLeadingSlash(o: String) =
    o match {
      case _ ⇒ o stripPrefix "/"
    }
}

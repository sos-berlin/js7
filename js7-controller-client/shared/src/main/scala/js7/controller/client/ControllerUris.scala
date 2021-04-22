package js7.controller.client

import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.common.http.Uris.{encodePath, encodeQuery}
import js7.controller.client.ControllerUris._
import js7.data.agent.AgentPath
import js7.data.event.{Event, EventId, EventRequest, JournalPosition}
import js7.data.order.OrderId
import js7.data.workflow.WorkflowPath
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

/**
 * URIs of the JS7 Controller.
 *
 * @author Joacim Zschimmer
 */
final class ControllerUris private(controllerUri: Uri)
{
  def overview = api()

  val command = api("/command")

  val session = api("/session")

  def events[E <: Event: ClassTag](request: EventRequest[E],
    heartbeat: Option[FiniteDuration] = None)
  : Uri =
    events_[E]("/event", request, heartbeat = heartbeat)

  private def events_[E <: Event: ClassTag](path: String, request: EventRequest[E], heartbeat: Option[FiniteDuration] = None) =
    Uri(
      api(path).string + encodeQuery(
       (heartbeat.map("heartbeat" -> _.toDecimalString)) ++
         request.toQueryParameters))

  def eventIds(timeout: Option[FiniteDuration], heartbeat: Option[FiniteDuration] = None) =
    Uri(
      api("/event").string + encodeQuery(
        Seq("onlyAcks" -> "true") ++
        timeout.map(o => "timeout" -> EventRequest.durationToString(o)) ++
        heartbeat.map("heartbeat" -> _.toDecimalString)))

  def clusterState = api("/cluster")

  def clusterNodeState = api("/cluster?return=ClusterNodeState")

  def journal(
    journalPosition: JournalPosition,
    heartbeat: Option[FiniteDuration] = None,
    timeout: Option[FiniteDuration] = None, markEOF: Boolean = false, returnAck: Boolean = false)
  = Uri(
    api("/journal").string + encodeQuery(
      (returnAck.thenList("return" -> "ack")) :::
      (heartbeat.map("heartbeat" -> _.toDecimalString)).toList :::
      (timeout.map("timeout" -> _.toDecimalString)).toList :::
      (markEOF.thenList("markEOF" -> "true")) :::
      ("file" -> journalPosition.fileEventId.toString) ::
      ("position" -> journalPosition.position.toString) :: Nil))

  object order {
    def overview = api("/order")

    def add = api("/order")

    def list[A: ClassTag]: Uri =
      api("/" + encodePath("order", ""), "return" -> encodeClass[A])

    def apply(orderId: OrderId): Uri =
      api("/" + encodePath("order", orderId.string))
  }

  object workflow {
    def apply(path: WorkflowPath): Uri =
      api("/" + encodePath("workflow", path.string))

    def list[A: ClassTag]: Uri =
      api("/" + encodePath("workflow", ""), "return" -> encodeClass[A])
  }

  object agent {
    def apply(path: AgentPath): Uri =
      api("/" + encodePath("agent", path.string))

    def list[A: ClassTag]: Uri =
      api("/" + encodePath("agent", ""), "return" -> encodeClass[A])
  }

  object snapshot {
    def list: Uri =
      list(None)

    def list(eventId: Option[EventId]) =
      api("/" + encodePath("snapshot", ""), eventId.toList.map("eventId" -> _.toString): _*)
  }

  def api(query: (String, String)*): Uri =
    api("", query: _*)

  def api(path: String, query: (String, String)*): Uri = {
    if (path.nonEmpty && !path.startsWith("/")) throw new IllegalArgumentException("Controller URI path must start with a slash")
    Uri(
      controller("api" + path).string + encodeQuery(query: _*))
  }

  def controller(path: String) = Uri(s"$controllerUri$path")

  override def toString = controllerUri.string
}

object ControllerUris
{
  def apply(controllerUri: Uri): ControllerUris =
    new ControllerUris(Uri(controllerUri.string.stripSuffix("/") + "/"))

  private def encodeClass[A: ClassTag]: String =
    encodeClass(implicitClass[A])

  private def encodeClass(cls: Class[_]): String = {
    require(cls != classOf[Nothing], "Missing return=CLASS")
    cls.simpleScalaName
  }
}

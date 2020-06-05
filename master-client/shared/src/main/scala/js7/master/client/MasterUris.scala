package js7.master.client

import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils._
import js7.base.utils.ScalazStyle._
import js7.base.web.Uri
import js7.common.http.Uris.{encodePath, encodeQuery}
import js7.data.agent.AgentRefPath
import js7.data.event.{Event, EventId, EventRequest}
import js7.data.fatevent.FatEvent
import js7.data.order.OrderId
import js7.data.workflow.WorkflowPath
import js7.master.client.MasterUris._
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

/**
 * URIs of the JS7 Master.
 *
 * @author Joacim Zschimmer
 */
final class MasterUris private(masterUri: Uri)
{
  def overview = api()

  val command = api("/command")

  val session = api("/session")

  def events[E <: Event: ClassTag](request: EventRequest[E], eventIdOnly: Boolean = false,
    heartbeat: Option[FiniteDuration] = None, onlyLastOfChunk: Boolean = false)
  : Uri =
    events_[E]("/event", request, eventIdOnly = eventIdOnly, heartbeat = heartbeat, onlyLastOfChunk = onlyLastOfChunk)

  def fatEvents[E <: FatEvent: ClassTag](request: EventRequest[E]): Uri =
    events_[E]("/fatEvent", request)

  private def events_[E <: Event: ClassTag](path: String, request: EventRequest[E],
    eventIdOnly: Boolean = false, heartbeat: Option[FiniteDuration] = None, onlyLastOfChunk: Boolean = false)
  = Uri(
      api(path).string + encodeQuery(
       (eventIdOnly thenVector ("eventIdOnly" -> "true")) ++
       (heartbeat.map("heartbeat" -> _.toDecimalString)) ++
       (onlyLastOfChunk thenVector ("onlyLastOfChunk" -> "true")) ++
         request.toQueryParameters))

  def clusterState = api("/cluster")

  def journal(fileEventId: EventId, position: Long, heartbeat: Option[FiniteDuration] = None,
    timeout: Option[FiniteDuration] = None, markEOF: Boolean = false, returnLength: Boolean = false)
  = Uri(
    api("/journal").string + encodeQuery(
      (returnLength.thenList("return" -> "length")) :::
      (heartbeat.map("heartbeat" -> _.toDecimalString)).toList :::
      (timeout.map("timeout" -> _.toDecimalString)).toList :::
      (markEOF.thenList("markEOF" -> "true")) :::
      ("file" -> fileEventId.toString) ::
      ("position" -> position.toString) :: Nil))

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
      api("/" + encodePath("workflow", path.withoutStartingSlash))

    def list[A: ClassTag]: Uri =
      api("/" + encodePath("workflow", ""), "return" -> encodeClass[A])
  }

  object agent {
    def apply(path: AgentRefPath): Uri =
      api("/" + encodePath("agent", path.withoutStartingSlash))

    def list[A: ClassTag]: Uri =
      api("/" + encodePath("agent", ""), "return" -> encodeClass[A])
  }

  object snapshot {
    val list: Uri =
      api("/" + encodePath("snapshot", ""))
  }

  def api(query: (String, String)*): Uri =
    api("", query: _*)

  def api(path: String, query: (String, String)*): Uri = {
    if (path.nonEmpty && !path.startsWith("/")) throw new IllegalArgumentException("Master URI path must start with a slash")
    Uri(
      master("api" + path).string + encodeQuery(query: _*))
  }

  def master(path: String) = Uri(s"$masterUri$path")

  override def toString = masterUri.string
}

object MasterUris
{
  def apply(masterUri: Uri): MasterUris =
    new MasterUris(Uri(masterUri.string.stripSuffix("/") + "/"))

  private def encodeClass[A: ClassTag]: String =
    encodeClass(implicitClass[A])

  private def encodeClass(cls: Class[_]): String = {
    require(cls != classOf[Nothing], "Missing return=CLASS")
    cls.simpleScalaName
  }
}

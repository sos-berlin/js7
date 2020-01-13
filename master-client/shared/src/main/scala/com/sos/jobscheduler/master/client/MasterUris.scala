package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.http.Uris.{encodePath, encodeQuery}
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest}
import com.sos.jobscheduler.data.fatevent.FatEvent
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.client.MasterUris._
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

/**
 * URIs of the JobScheduler Master.
 *
 * @author Joacim Zschimmer
 */
final class MasterUris private(masterUri: String)
{
  def overview = api()

  val command = api("/command")

  val session = api("/session")

  def events[E <: Event: ClassTag](request: EventRequest[E], eventIdOnly: Boolean = false,
    heartbeat: Option[FiniteDuration] = None, onlyLastOfChunk: Boolean = false)
  : String =
    events_[E]("/event", request, eventIdOnly = eventIdOnly, heartbeat = heartbeat, onlyLastOfChunk = onlyLastOfChunk)

  def fatEvents[E <: FatEvent: ClassTag](request: EventRequest[E]): String =
    events_[E]("/fatEvent", request)

  private def events_[E <: Event: ClassTag](path: String, request: EventRequest[E],
    eventIdOnly: Boolean = false, heartbeat: Option[FiniteDuration] = None, onlyLastOfChunk: Boolean = false)
  : String =
    api(path) + encodeQuery(
     (eventIdOnly thenVector ("eventIdOnly" -> "true")) ++
     (heartbeat.map("heartbeat" -> _.toDecimalString)) ++
     (onlyLastOfChunk thenVector ("onlyLastOfChunk" -> "true")) ++
       request.toQueryParameters)

  def journal(fileEventId: EventId, position: Long, heartbeat: Option[FiniteDuration] = None,
    timeout: FiniteDuration, markEOF: Boolean = false, returnLength: Boolean = false)
  : String =
    api("/journal") + encodeQuery(
      (returnLength.thenList("return" -> "length")) :::
      (heartbeat.map("heartbeat" -> _.toDecimalString)).toList :::
      ("timeout" -> timeout.toDecimalString) ::
      (markEOF.thenList("markEOF" -> "true")) :::
      ("file" -> fileEventId.toString) ::
      ("position" -> position.toString) :: Nil)

  object order {
    def overview = api("/order")

    def add = api("/order")

    def list[A: ClassTag]: String =
      api("/" + encodePath("order", ""), "return" -> encodeClass[A])

    def apply(orderId: OrderId): String =
      api("/" + encodePath("order", orderId.string))
  }

  object workflow {
    def apply(path: WorkflowPath): String =
      api("/" + encodePath("workflow", path.withoutStartingSlash))

    def list[A: ClassTag]: String =
      api("/" + encodePath("workflow", ""), "return" -> encodeClass[A])
  }

  object agent {
    def apply(path: AgentRefPath): String =
      api("/" + encodePath("agent", path.withoutStartingSlash))

    def list[A: ClassTag]: String =
      api("/" + encodePath("agent", ""), "return" -> encodeClass[A])
  }

  object snapshot {
    val list: String =
      api("/" + encodePath("snapshot", ""))
  }

  def api(query: (String, String)*): String =
    api("", query: _*)

  def api(path: String, query: (String, String)*): String = {
    if (path.nonEmpty && !path.startsWith("/")) throw new IllegalArgumentException("Master URI path must start with a slash")
    master("api" + path) +
      encodeQuery(query: _*)
  }

  def master(path: String) = s"$masterUri$path"

  override def toString = masterUri
}

object MasterUris
{
  def apply(masterUri: String): MasterUris =
    new MasterUris(
      if (masterUri.isEmpty)
        masterUri
      else
        masterUri.stripSuffix("/") + "/")

  private def encodeClass[A: ClassTag]: String =
    encodeClass(implicitClass[A])

  private def encodeClass(cls: Class[_]): String = {
    require(cls != classOf[Nothing], "Missing return=CLASS")
    cls.simpleScalaName
  }
}

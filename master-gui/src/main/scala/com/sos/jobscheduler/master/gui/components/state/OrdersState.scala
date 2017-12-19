package com.sos.jobscheduler.master.gui.components.state

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.event.{EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.master.gui.common.Utils._
import com.sos.jobscheduler.master.gui.components.state.OrdersState._
import org.scalajs.dom
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final case class OrdersState(
  content: Content,
  step: Int,
  error: Option[String])
{
  def updateOrders(stamped: Stamped[Seq[Order[Order.State]]]): OrdersState = {
    val orders = stamped.value
    val updatedIdToEntry = orders.map(v ⇒ v.id → OrderEntry(v)).toMap
    copy(
      content = OrdersState.FetchedContent(
        idToEntry = updatedIdToEntry,
        sequence = updatedIdToEntry.keys.toArray.sorted(reverseOrdering),
        eventId = stamped.eventId, eventCount = 0),
      error = None,
      step = step + 1)
  }

  def maybeOrderEntry(orderId: OrderId): Option[OrderEntry] =
    content match {
      case content: FetchedContent ⇒ content.idToEntry.get(orderId)
      case _ ⇒ None
    }
}

object OrdersState {
  val Empty = OrdersState(Initial, step = 0, error = None)
  private val reverseOrdering = implicitly[Ordering[OrderId]].reverse

  sealed trait Content

  object Initial extends Content
  object FetchingContent extends Content

  final case class FetchedContent(
    idToEntry: Map[OrderId, OrderEntry],
    sequence: Array[OrderId],  // immutable! For performance only
    eventId: EventId,
    eventCount: Int)
  extends Content
  {
    def handleEvents(stampedEvents: Seq[Stamped[KeyedEvent[OrderEvent]]]): FetchedContent = {
      val updated = mutable.Map[OrderId, OrderEntry]()
      var added = List[OrderId]()
      var evtCount = 0
      val nowMillis = Timestamp.epochMilli
      stampedEvents foreach {
        case Stamped(_, KeyedEvent(orderId, event: OrderAdded)) ⇒
          updated += orderId → OrderEntry(Order.fromOrderAdded(orderId, event), updatedAt = nowMillis)
          added = orderId :: added
          evtCount += 1

        case Stamped(eId, KeyedEvent(orderId, event: OrderEvent)) ⇒
          updated.get(orderId) orElse idToEntry.get(orderId) match {
            case None ⇒ dom.console.error("Unknown OrderId: " + eventToLog(eId, orderId, event))
            case Some(entry) ⇒
              updated += orderId → (event match {
                case event: OrderCoreEvent ⇒
                  entry.copy(
                    order = entry.order.update(event),
                    updatedAt = nowMillis)

                case OrderStdWritten(t, chunk) ⇒
                  entry.copy(
                    output = entry.output ++ splitLines(chunk, s"$t: "),
                    updatedAt = nowMillis)
              })
              evtCount += 1
          }

        case Stamped(eId, KeyedEvent(orderId, event)) ⇒
          dom.console.warn("Ignored: " + eventToLog(eId, orderId, event))

        case _ ⇒
      }
      val updatedIdToEntry = idToEntry ++ updated
      copy(
        idToEntry = updatedIdToEntry,
        sequence = if (added.nonEmpty) added.toArray ++ sequence else sequence,
        eventId = stampedEvents.last.eventId,
        eventCount = eventCount + evtCount)
    }
  }

  private def splitLines(chunk: String, linePrefix: String) =
    chunk.split("\n").map(line ⇒ s"$linePrefix$line")

  //private def lastLineOfChunk(chunk: String): String = {
  //  val c = chunk.trim
  //  val start = c.lastIndexOf("\n") match {
  //    case -1 ⇒ 0
  //    case i ⇒ i + 1
  //  }
  //  c.substring(start).truncateWithEllipsis(50, showLength = false)
  //}

  final case class OrderEntry(
    order: Order[Order.State],
    output: Vector[String] = Vector(),
    updatedAt: Long = 0)
  {
    def id = order.id
  }
}



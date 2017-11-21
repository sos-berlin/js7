package com.sos.jobscheduler.master.gui.components.state

import com.sos.jobscheduler.master.gui.components.Utils._
import com.sos.jobscheduler.master.gui.components.state.OrdersState._
import com.sos.jobscheduler.master.gui.data.OrderEvent.{OrderAdded, OrderCoreEvent}
import com.sos.jobscheduler.master.gui.data.event.{EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.master.gui.data.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.master.gui.services.MasterApi
import org.scalajs.dom
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final case class OrdersState(
  content: Content,
  step: Int,
  error: Option[MasterApi.Error])
{
  def updateOrders(stamped: Stamped[Seq[Order[Order.State]]]): OrdersState = {
    val orders = stamped.value
    copy(
      content = OrdersState.FetchedContent(
        idToOrder = orders.map(v ⇒ v.id → Entry(v)).toMap,
        sequence = orders.map(_.id).sorted.reverse.toList,
        eventId = stamped.eventId, eventCount = 0),
      error = None,
      step = step + 1)
  }
}

object OrdersState {
  val Empty = OrdersState(StillFetchingContent, step = 0, error = None)
  sealed trait Content

  object StillFetchingContent extends Content

  final case class FetchedContent(
    idToOrder: Map[OrderId, Entry],
    sequence: List[OrderId],
    eventId: EventId,
    eventCount: Int)
  extends Content
  {
    def handleEvents(stampedEvents: Seq[Stamped[KeyedEvent[OrderEvent]]]): FetchedContent = {
      val updatedOrders = mutable.Map[OrderId, Entry]()
      val addedOrders = mutable.Buffer[OrderId]()
      var evtCount = 0
      stampedEvents foreach {
        case Stamped(_, KeyedEvent(orderId, event: OrderAdded)) ⇒
          updatedOrders += orderId → Entry(Order.fromOrderAdded(orderId, event), isUpdated = true)
          addedOrders += orderId
          evtCount += 1

        case Stamped(eId, KeyedEvent(orderId, event: OrderCoreEvent)) ⇒
          updatedOrders.get(orderId) orElse idToOrder.get(orderId) match {
            case Some(entry) ⇒
              updatedOrders += orderId → Entry(entry.order.update(event), isUpdated = true)
              evtCount += 1
            case None ⇒
              dom.console.error("Unknown OrderId: " + eventToLog(eId, orderId, event))
          }

        case Stamped(eId, KeyedEvent(orderId, event)) ⇒
          dom.console.warn("Ignored: " + eventToLog(eId, orderId, event))

        case _ ⇒
      }
      copy(
        idToOrder = idToOrder ++ updatedOrders,
        sequence = addedOrders.reverseIterator ++: sequence,
        eventId = stampedEvents.last.eventId,
        eventCount = eventCount + evtCount)
    }
  }

  final case class Entry(order: Order[Order.State], isUpdated: Boolean = false) {
    def id = order.id
  }
}



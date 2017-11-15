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

object OrdersState {
  val Empty = OrdersState(StillFetchingContent, step = 0, error = None)
  sealed trait Content

  object StillFetchingContent extends Content

  final case class FetchedContent(
    idToOrder: Map[OrderId, Order[Order.State]],
    sequence: List[OrderId],
    eventId: EventId,
    eventCount: Int)
  extends Content
  {
    def handleEvents(stampedEvents: Seq[Stamped[KeyedEvent[OrderEvent]]]): FetchedContent = {
      val updatedOrders = mutable.Map[OrderId, Order[Order.State]]()
      val addedOrders = mutable.Buffer[OrderId]()
      var evtCount = 0
      stampedEvents foreach {
        case Stamped(_, KeyedEvent(orderId, event: OrderAdded)) ⇒
          updatedOrders += orderId → Order.fromOrderAdded(orderId, event)
          addedOrders += orderId
          evtCount += 1

        case Stamped(eId, KeyedEvent(orderId, event: OrderCoreEvent)) ⇒
          updatedOrders.get(orderId) orElse idToOrder.get(orderId) match {
            case Some(order) ⇒
              updatedOrders += orderId → order.update(event)
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
}



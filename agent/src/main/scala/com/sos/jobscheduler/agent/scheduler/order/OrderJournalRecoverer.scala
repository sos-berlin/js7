package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.ActorRef
import com.sos.jobscheduler.agent.scheduler.event.EventQueue
import com.sos.jobscheduler.common.scalautil.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.jobnet.{Jobnet, JobnetEvent, JobnetPath}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderCoreEvent, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer
import java.nio.file.Path
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[order] final class OrderJournalRecoverer(protected val journalFile: Path, eventsForMaster: ActorRef)
  (implicit self: ActorRef)
extends JsonJournalRecoverer[Event] {

  protected val jsonJournalMeta = AgentOrderKeeper.MyJournalMeta
  private val jobnetRegister = new JobnetRegister
  private val idToOrder = mutable.Map[OrderId, Order[Order.State]]()

  protected def recoverSnapshot = {
    case jobnet: Jobnet ⇒
      jobnetRegister.recover(jobnet)

    case order: Order[Order.State] ⇒
      idToOrder.insert(order.id → order)

    case snapshot: EventQueue.Snapshot ⇒
      eventsForMaster ! snapshot
  }

  protected def recoverEvent = {
    case Stamped(_, KeyedEvent(path: JobnetPath, event: JobnetEvent.JobnetAttached)) ⇒
      jobnetRegister.handleEvent(KeyedEvent(event)(path))

    case stamped @ Stamped(_, KeyedEvent(orderId: OrderId, event: OrderEvent)) ⇒
      handleEvent(orderId, event)
      eventsForMaster ! stamped
  }

  private def handleEvent(orderId: OrderId, event: OrderEvent) =
    event match {
      case event: OrderEvent.OrderAttached ⇒
        idToOrder.insert(orderId → Order.fromOrderAttached(orderId, event))

      case OrderEvent.OrderDetached ⇒
        idToOrder -= orderId

      case event: OrderEvent ⇒
        // See also OrderActor#update
        event match {
          case event: OrderCoreEvent ⇒
            idToOrder(orderId) = idToOrder(orderId).update(event)
          case _: OrderStdWritten ⇒
            // OrderStdWritten is not handled (but forwarded to Master)
        }
    }

  def jobnets = jobnetRegister.jobnets
  def orders = idToOrder.values
}

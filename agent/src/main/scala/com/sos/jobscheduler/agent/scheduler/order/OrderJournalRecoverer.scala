package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import com.softwaremill.tagging.@@
import com.sos.jobscheduler.agent.scheduler.event.EventQueueActor
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.jobnet.{Jobnet, JobnetEvent, JobnetPath}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderCoreEvent, OrderDetached, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer
import java.nio.file.Path
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[order] final class OrderJournalRecoverer(protected val journalFile: Path, eventsForMaster: ActorRef @@ EventQueueActor)
(implicit askTimeout: Timeout)
extends JsonJournalRecoverer[Event] {

  protected val jsonJournalMeta = AgentOrderKeeper.MyJournalMeta
  private val jobnetRegister = new JobnetRegister
  private val idToOrder = mutable.Map[OrderId, Order[Order.State]]()

  protected def recoverSnapshot = {
    case jobnet: Jobnet ⇒
      jobnetRegister.recover(jobnet)

    case order: Order[Order.State] ⇒
      idToOrder.insert(order.id → order)

    case snapshot: EventQueueActor.Snapshot ⇒
      eventsForMaster ! snapshot
  }

  protected def recoverEvent = {
    case Stamped(_, KeyedEvent(path: JobnetPath, event: JobnetEvent.JobnetAttached)) ⇒
      jobnetRegister.handleEvent(KeyedEvent(event)(path))

    case stamped @ Stamped(_, KeyedEvent(orderId: OrderId, event: OrderEvent)) ⇒
      event match {
        case OrderDetached ⇒
          (eventsForMaster ? stamped) await 2 * askTimeout.duration.toJavaDuration  // blocking !!!
        case _ ⇒
          eventsForMaster ! stamped
      }
      handleEvent(orderId, event)
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

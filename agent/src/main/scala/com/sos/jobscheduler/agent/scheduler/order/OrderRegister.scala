package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.ActorRef
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.{Timer, TimerService}
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAttached, OrderDetached}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.shared.common.ActorRegister
import java.time.Instant
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
private[order] final class OrderRegister(timerService: TimerService) extends ActorRegister[OrderId, OrderEntry](_.actor) {

  def recover(order: Order[Order.State], actor: ActorRef): Unit = {
    insert(order.id → new OrderEntry(order, actor))
  }

  def handleOrderDetached(keyedEvent: KeyedEvent[OrderDetached.type]): Unit = {
    this -= keyedEvent.key
  }

  def insert(order: Order[Order.State], actor: ActorRef): Unit = {
    insert(order.id → new OrderEntry(order, actor))
  }

  def onActorTerminated(actor: ActorRef)(implicit timerService: TimerService): Unit = {
    for (orderEntry ← remove(actorToKey(actor))) {
      logger.debug(s"Removing ${orderEntry.order.id} after Actor death")
      orderEntry.timer foreach timerService.cancel
    }
  }
}

private[order] object OrderRegister {
  private def logger = Logger(getClass)

  final class OrderEntry(
    private var _order: Order[Order.State],
    val actor: ActorRef)
  {
    var detaching: Boolean = false
    private[OrderRegister] var timer: Option[Timer[Unit]] = None

    def order = _order
    def order_=(o: Order[Order.State]) = _order = o

    def at(instant: Instant)(body: ⇒ Unit)(implicit timerService: TimerService, ec: ExecutionContext): Unit = {
      val t = timerService.at(instant, name = order.id.string)
      t onElapsed {
        timer = None
        body
      }
      timer foreach timerService.cancel
      timer = Some(t)
    }
  }
}

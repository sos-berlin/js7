package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.ActorRef
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.{Timer, TimerService}
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.jobnet.{JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAttached, OrderDetached, OrderNodeChanged}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.shared.common.ActorRegister
import java.time.Instant
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
private[order] final class OrderRegister(timerService: TimerService) extends ActorRegister[OrderId, OrderEntry](_.actor) {

  def handleOrderAttached(keyedEvent: KeyedEvent[OrderAttached], actor: ActorRef): Unit = {
    val orderId = keyedEvent.key
    val event = keyedEvent.event
    insert(orderId → new OrderEntry(orderId, actor, event.nodeKey.jobnetPath, event.nodeKey.nodeId))
  }

  def handleOrderDetached(keyedEvent: KeyedEvent[OrderDetached.type]): Unit = {
    this -= keyedEvent.key
  }

  def handleOrderNodeChanged(keyedEvent: KeyedEvent[OrderNodeChanged]): Unit = {
    apply(keyedEvent.key).nodeId = keyedEvent.event.nodeId
  }

  def insert(order: Order[Order.State], actor: ActorRef): Unit = {
    insert(order.id → new OrderEntry(order.id, actor, order.jobnetPath, order.nodeId))
  }

  def onActorTerminated(actor: ActorRef)(implicit timerService: TimerService): Unit = {
    for (orderEntry ← remove(actorToKey(actor))) {
      logger.debug(s"Removing ${orderEntry.orderId} after Actor death")
      orderEntry.timer foreach timerService.cancel
    }
  }
}

private[order] object OrderRegister {
  private def logger = Logger(getClass)

  final class OrderEntry(
    val orderId: OrderId,
    val actor: ActorRef,
    val jobnetPath: JobnetPath,
    var nodeId: NodeId)
  {
    private[OrderRegister] var timer: Option[Timer[Unit]] = None

    def nodeKey = NodeKey(jobnetPath, nodeId)

    def at(instant: Instant)(body: ⇒ Unit)(implicit timerService: TimerService, ec: ExecutionContext): Unit = {
      val t = timerService.at(instant, name = orderId.string)
      t onElapsed {
        timer = None
        body
      }
      timer foreach timerService.cancel
      timer = Some(t)
    }
  }
}

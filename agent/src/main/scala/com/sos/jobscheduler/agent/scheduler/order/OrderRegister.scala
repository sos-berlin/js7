package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.ActorRef
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister._
import com.sos.jobscheduler.common.time.timer.{Timer, TimerService}
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.jobnet.Jobnet
import com.sos.jobscheduler.data.jobnet.Jobnet.{JobNode, Node}
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetached
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.shared.common.ActorRegister
import java.time.Instant
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
private[order] final class OrderRegister(timerService: TimerService) extends ActorRegister[OrderId, OrderEntry](_.actor) {

  def recover(order: Order[Order.State], jobnet: Jobnet, actor: ActorRef): OrderEntry = {
    val orderEntry = new OrderEntry(order, jobnet, actor)
    insert(order.id → orderEntry)
    orderEntry
  }

  def handleOrderDetached(keyedEvent: KeyedEvent[OrderDetached.type]): Unit = {
    this -= keyedEvent.key
  }

  def insert(order: Order[Order.State], jobnet: Jobnet, actor: ActorRef): Unit = {
    insert(order.id → new OrderEntry(order, jobnet, actor))
  }

  def onActorTerminated(actor: ActorRef)(implicit timerService: TimerService): Unit = {
    for (orderEntry ← remove(actorToKey(actor))) {
      orderEntry.timer foreach timerService.cancel
    }
  }
}

private[order] object OrderRegister {

  final class OrderEntry(
    private var _order: Order[Order.State],
    private val jobnet: Jobnet,
    val actor: ActorRef)
  {
    assert(jobnet.path == _order.jobnetPath)

    var detaching: Boolean = false
    private[OrderRegister] var timer: Option[Timer[Unit]] = None

    def order = _order

    def order_=(o: Order[Order.State]) = {
      assert(jobnet.path == o.jobnetPath)
      _order = o
    }

    def jobNodeOption: Option[JobNode] =
      jobnet.jobNodeOption(order.nodeId)

    def jobNode: JobNode =
      jobnet.jobNode(order.nodeId)

    def nodeOption: Option[Node] =
      jobnet.idToNode.get(order.nodeId)

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

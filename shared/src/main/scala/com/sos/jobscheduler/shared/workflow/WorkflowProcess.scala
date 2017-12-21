package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.OrderTransitionedEvent
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowGraph
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.shared.workflow.Transitions.ExecutableTransition
import com.sos.jobscheduler.shared.workflow.WorkflowProcess._
import com.sos.jobscheduler.shared.workflow.Workflows.ExecutableWorkflowGraph
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class WorkflowProcess(workflowGraph: WorkflowGraph, idToOrder: PartialFunction[OrderId, Order[Order.State]]) {

  def guardedSwitchTransition(order: Order[Order.Transitionable]): Option[KeyedEvent[OrderTransitionedEvent]] =
    for {
      transition ← workflowGraph.transitionForNode(order.nodeId, order.state)
      keyedEvent ← guardedSwitchTransition(order, transition)
    } yield keyedEvent

  def guardedSwitchTransition(order: Order[Order.Transitionable], transition: Transition): Option[KeyedEvent[OrderTransitionedEvent]] =
    try switchTransition(order, transition)
    catch {
      case NonFatal(t) ⇒
        logger.warn(s"Error when applying $transition to '${order.id}' ${t.toStringWithCauses}", t)
        None
    }

  def switchTransition(order: Order[Order.Transitionable], transition: Transition): Option[KeyedEvent[OrderTransitionedEvent]] =
    transition.switch(transition.joinableOrders(order, idToOrder).toKeyedMap(_.nodeId))
}

object WorkflowProcess {
  private val logger = Logger(getClass)
}

package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.data.order.{Order, OrderEvent}
import com.sos.jobscheduler.data.workflow.Workflow
import org.scalactic.Requirements._

/**
  * @author Joacim Zschimmer
  */
object Transitions {

  def switch(order: Order[Order.Processed.type], workflow: Workflow): Option[OrderEvent.OrderTransitioned] = {
    require(order.workflowPath == workflow.path)
    workflow.nodeToTransition.get(order.nodeId) flatMap { transition ⇒
      require(transition.fromNodeIds == List(order.nodeId))
      transition.switch(Map(order.nodeId → order.toLean))
    }
  }
}

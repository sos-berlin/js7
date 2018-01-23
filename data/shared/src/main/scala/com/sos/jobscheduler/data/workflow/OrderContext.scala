package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.order.{Order, OrderId}

/**
  * @author Joacim Zschimmer
  */
trait OrderContext {

  def idToOrder: PartialFunction[OrderId, Order[Order.State]]

  def childOrderEnded(order: Order[Order.State]): Boolean

  def instruction(workflowPosition: WorkflowPosition): Instruction
}

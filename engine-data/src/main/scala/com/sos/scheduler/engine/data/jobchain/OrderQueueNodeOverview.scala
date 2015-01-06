package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.order.OrderState

trait OrderQueueNodeOverview extends NodeOverview {
  def nextState: OrderState
  def errorState: OrderState
  def action: JobChainNodeAction
  def orderCount: Int
}

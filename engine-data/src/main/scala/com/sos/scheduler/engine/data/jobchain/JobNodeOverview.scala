package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.order.OrderState

/**
 * @author Joacim Zschimmer
 */
trait JobNodeOverview extends NodeOverview {
  def nextState: OrderState
  def errorState: OrderState
  def action: JobChainNodeAction
  def orderCount: Int
}

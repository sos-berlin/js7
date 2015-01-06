package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.order.OrderState

/**
 * @author Joacim Zschimmer
 */
final case class NodeKey(jobChainPath: JobChainPath, state: OrderState)

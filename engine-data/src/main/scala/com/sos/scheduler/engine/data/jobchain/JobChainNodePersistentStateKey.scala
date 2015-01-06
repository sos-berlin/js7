package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.order.OrderState

case class JobChainNodePersistentStateKey(jobChainPath: JobChainPath, state: OrderState)

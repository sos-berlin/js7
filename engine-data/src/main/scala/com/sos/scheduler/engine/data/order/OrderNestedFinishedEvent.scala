package com.sos.scheduler.engine.data.order

/**
 * @author Joacim Zschimmer
 */
final case class OrderNestedFinishedEvent(orderKey: OrderKey)
extends OrderEvent

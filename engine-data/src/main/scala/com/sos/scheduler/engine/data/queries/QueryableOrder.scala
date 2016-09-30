package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.order.{OrderKey, OrderProcessingState, OrderSourceType}
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
trait QueryableOrder {
  def orderKey: OrderKey
  def sourceType: OrderSourceType
  def isSuspended: Boolean
  def isSetback: Boolean
  def isBlacklisted: Boolean
  def processingStateClass: Class[_ <: OrderProcessingState]
}

object QueryableOrder {
  @TestOnly
  final case class ForTest(
    orderKey: OrderKey,
    sourceType: OrderSourceType = OrderSourceType.AdHoc,
    isSetback: Boolean = false,
    isBlacklisted: Boolean = false,
    isSuspended: Boolean = false,
    processingStateClass: Class[_ <: OrderProcessingState] = OrderProcessingState.NotPlanned.getClass)
  extends QueryableOrder
}

package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.order.{OrderKey, OrderSourceType}
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
}

object QueryableOrder {
  @TestOnly
  final case class ForTest(
    orderKey: OrderKey,
    sourceType: OrderSourceType = OrderSourceType.AdHoc,
    isSetback: Boolean = false,
    isBlacklisted: Boolean = false,
    isSuspended: Boolean = false)
  extends QueryableOrder
}

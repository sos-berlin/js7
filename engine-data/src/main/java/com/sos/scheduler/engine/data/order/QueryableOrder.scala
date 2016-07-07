package com.sos.scheduler.engine.data.order

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
    sourceType: OrderSourceType = OrderSourceType.adHoc,
    isSetback: Boolean = false,
    isBlacklisted: Boolean = false,
    isSuspended: Boolean = false)
  extends QueryableOrder
}

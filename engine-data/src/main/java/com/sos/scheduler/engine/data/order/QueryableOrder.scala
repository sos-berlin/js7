package com.sos.scheduler.engine.data.order

/**
  * @author Joacim Zschimmer
  */
trait QueryableOrder {
  def sourceType: OrderSourceType
  def isSetback: Boolean
  def isBlacklisted: Boolean
  def isSuspended: Boolean
}

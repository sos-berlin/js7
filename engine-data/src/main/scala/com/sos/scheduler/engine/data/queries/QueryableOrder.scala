package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.jobchain.NodeId
import com.sos.scheduler.engine.data.order.{OrderKey, OrderProcessingState, OrderSourceType}
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
trait QueryableOrder {
  def orderKey: OrderKey
  def nodeId: NodeId
  def sourceType: OrderSourceType
  def isSuspended: Boolean
  def isSetback: Boolean
  def isBlacklisted: Boolean
  def orderProcessingStateClass: Class[_ <: OrderProcessingState]
}

object QueryableOrder {
  final case class Standard(
    orderKey: OrderKey,
    nodeId: NodeId,
    sourceType: OrderSourceType,
    isSetback: Boolean,
    isBlacklisted: Boolean,
    isSuspended: Boolean,
    orderProcessingStateClass: Class[_ <: OrderProcessingState])
  extends QueryableOrder

  @TestOnly
  final case class ForTest(
    orderKey: OrderKey,
    nodeId: NodeId,
    sourceType: OrderSourceType = OrderSourceType.AdHoc,
    isSetback: Boolean = false,
    isBlacklisted: Boolean = false,
    isSuspended: Boolean = false,
    orderProcessingStateClass: Class[_ <: OrderProcessingState] = OrderProcessingState.NotPlanned.getClass)
  extends QueryableOrder
}

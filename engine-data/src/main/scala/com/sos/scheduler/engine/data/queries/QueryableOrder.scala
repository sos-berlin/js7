package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import com.sos.scheduler.engine.data.jobchain.NodeId
import com.sos.scheduler.engine.data.order.{OrderKey, OrderProcessingState, OrderSourceType}
import org.jetbrains.annotations.TestOnly
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait QueryableOrder {
  def orderKey: OrderKey
  def nodeId: NodeId
  def orderSourceType: OrderSourceType
  def isSuspended: Boolean
  def isSetback: Boolean
  def isBlacklisted: Boolean
  def orderProcessingStateClass: Class[_ <: OrderProcessingState]

  final def isOrderProcessingState[A <: OrderProcessingState: ClassTag]: Boolean =
    implicitClass[A] isAssignableFrom orderProcessingStateClass
}

object QueryableOrder {
  final case class Standard(
    orderKey: OrderKey,
    nodeId: NodeId,
    orderSourceType: OrderSourceType,
    isSetback: Boolean,
    isBlacklisted: Boolean,
    isSuspended: Boolean,
    orderProcessingStateClass: Class[_ <: OrderProcessingState])
  extends QueryableOrder

  @TestOnly
  final case class ForTest(
    orderKey: OrderKey,
    nodeId: NodeId,
    orderSourceType: OrderSourceType = OrderSourceType.AdHoc,
    isSetback: Boolean = false,
    isBlacklisted: Boolean = false,
    isSuspended: Boolean = false,
    orderProcessingStateClass: Class[_ <: OrderProcessingState] = OrderProcessingState.NotPlanned.getClass)
  extends QueryableOrder
}

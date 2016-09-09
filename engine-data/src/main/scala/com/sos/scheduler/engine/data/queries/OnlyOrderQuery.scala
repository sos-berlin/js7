package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.order.{OrderId, OrderProcessingState, OrderSourceType}

/**
  * @author Joacim Zschimmer
  */
trait OnlyOrderQuery {
  def orderId: Option[OrderId]
  def isSuspended: Option[Boolean]
  def isSetback: Option[Boolean]
  def isBlacklisted: Option[Boolean]
  def isOrderSourceType: Option[Set[OrderSourceType]]
  def isOrderProcessingState: Option[Set[Class[_ <: OrderProcessingState]]]

  final def matchesOrder(o: QueryableOrder) =
    (orderId forall { _ == o.orderKey.id }) &&
    (isSuspended forall { _ == o.isSuspended }) &&
    (isSetback forall { _ == o.isSetback }) &&
    (isBlacklisted forall { _ == o.isBlacklisted }) &&
    (isOrderSourceType forall { _ contains o.sourceType }) &&
    (isOrderProcessingState forall { _ exists { _ isAssignableFrom o.processingStateClass }})
}

object OnlyOrderQuery {
  val AllOrderSourceTypes = OrderSourceType.values.toSet
  val All: OnlyOrderQuery = Standard()
  private implicit val OrderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat

  final case class Standard(
    orderId: Option[OrderId] = None,
    isSuspended: Option[Boolean] = None,
    isSetback: Option[Boolean] = None,
    isBlacklisted: Option[Boolean] = None,
    isOrderSourceType: Option[Set[OrderSourceType]] = None,
    isOrderProcessingState: Option[Set[Class[_ <: OrderProcessingState]]] = None)
  extends OnlyOrderQuery
}

package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.order.{OrderSourceType, QueryableOrder}

/**
  * @author Joacim Zschimmer
  */
trait OnlyOrderQuery {
  def isSuspended: Option[Boolean]
  def isSetback: Option[Boolean]
  def isBlacklisted: Option[Boolean]
  def isOrderSourceType: Option[Set[OrderSourceType]]

  final def matchesOrder(o: QueryableOrder) =
    (isSuspended forall { _ == o.isSuspended }) &&
    (isSetback forall { _ == o.isSetback }) &&
    (isBlacklisted forall { _ == o.isBlacklisted }) &&
    (isOrderSourceType forall { _ contains o.sourceType })
}

object OnlyOrderQuery {
  val AllOrderSourceTypes = OrderSourceType.values.toSet
  val All: OnlyOrderQuery = Standard()
  private implicit val OrderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat

  final case class Standard(
    isSuspended: Option[Boolean] = None,
    isSetback: Option[Boolean] = None,
    isBlacklisted: Option[Boolean] = None,
    isOrderSourceType: Option[Set[OrderSourceType]] = None)
  extends OnlyOrderQuery
}

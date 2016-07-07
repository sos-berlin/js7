package com.sos.scheduler.engine.data.order

import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderQuery(
  isSuspended: Option[Boolean] = None,
  isSetback: Option[Boolean] = None,
  isBlacklisted: Option[Boolean] = None,
  isSourceType: Option[Set[OrderSourceType]] = None)
extends (QueryableOrder ⇒ Boolean) {

  def apply(o: QueryableOrder) =
    (isSuspended forall { _ == o.isSuspended }) &&
    (isSetback forall { _ == o.isSetback }) &&
    (isBlacklisted forall { _ == o.isBlacklisted }) &&
    (isSourceType forall { _ contains o.sourceType })
}

object OrderQuery {
  val AllOrderSourceTypes = OrderSourceType.values.toSet
  val All = new OrderQuery()
  private implicit val OrderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat4(apply)
}


package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.order.{OrderId, OrderProcessingState, OrderSourceType}

/**
  * @author Joacim Zschimmer
  */
trait OnlyOrderQuery {
  def orderIds: Option[Set[OrderId]]
  def jobPaths: Option[Set[JobPath]]
  def isSuspended: Option[Boolean]
  def isSetback: Option[Boolean]
  def isBlacklisted: Option[Boolean]
  def isOrderSourceType: Option[Set[OrderSourceType]]
  def isOrderProcessingState: Option[Set[Class[_ <: OrderProcessingState]]]
  /** Or-ed with `isOrderProcessingState`, such that `suspended` can be seen as a `OrderProcessingState`. */
  def orIsSuspended: Boolean

  final def matchesOrder(o: QueryableOrder, jobPathOption: Option[JobPath] = None) =
    (orderIds forall { _ contains o.orderKey.id }) &&
    (isSuspended forall { _ == o.isSuspended }) &&
    (isSetback forall { _ == o.isSetback }) &&
    (isBlacklisted forall { _ == o.isBlacklisted }) &&
    (isOrderSourceType forall { _ contains o.sourceType }) &&
    ((isOrderProcessingState forall { _ exists { _ isAssignableFrom o.processingStateClass }})
      || orIsSuspended && o.isSuspended) &&
    (jobPaths forall { set ⇒ jobPathOption exists set.contains })
}

object OnlyOrderQuery {
  val AllOrderSourceTypes = OrderSourceType.values.toSet
  val All: OnlyOrderQuery = Standard()
  private implicit val OrderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat

  final case class Standard(
    orderIds: Option[Set[OrderId]] = None,
    jobPaths: Option[Set[JobPath]] = None,
    isSuspended: Option[Boolean] = None,
    isSetback: Option[Boolean] = None,
    isBlacklisted: Option[Boolean] = None,
    isOrderSourceType: Option[Set[OrderSourceType]] = None,
    isOrderProcessingState: Option[Set[Class[_ <: OrderProcessingState]]] = None,
    orIsSuspended: Boolean = false)
  extends OnlyOrderQuery
}

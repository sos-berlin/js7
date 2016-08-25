package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.order.{OrderKey, OrderSourceType}
import com.sos.scheduler.engine.data.queries.OrderQuery._
import scala.collection.JavaConversions._

/**
  * @author Joacim Zschimmer
  */
final case class OrderQuery(
  jobChainPathQuery: PathQuery = PathQuery.All,
  orderIdQuery: OrderIdQuery = OrderIdQuery.All,
  isDistributed: Option[Boolean] = None,
  isSuspended: Option[Boolean] = None,
  isSetback: Option[Boolean] = None,
  isBlacklisted: Option[Boolean] = None,
  isOrderSourceType: Option[Set[OrderSourceType]] = None,
  notInTaskLimitPerNode: Option[Int] = None)
extends OnlyOrderQuery with JobChainQuery {

  for (limit ← notInTaskLimitPerNode) require(limit >= 0, s"Invalid notInTaskLimitPerNode=$notInTaskLimitPerNode")

  def withJobChainPathQuery(q: PathQuery) = copy(jobChainPathQuery = q)
  def withOrderKey(orderKey: OrderKey) = copy(
    jobChainPathQuery = PathQuery(orderKey.jobChainPath),
    orderIdQuery = OrderIdQuery(Some(orderKey.id)))
  def withIsDistributed(o: Boolean) = copy(isDistributed = Some(o))
  def withIsSuspended(o: Boolean) = copy(isSuspended = Some(o))
  def withIsSetback(o: Boolean) = copy(isSetback = Some(o))
  def withIsBlacklisted(o: Boolean) = copy(isBlacklisted = Some(o))
  def withOrderSourceTypes(o: java.util.List[OrderSourceType]) = copy(isOrderSourceType = Some(o.toSet))
  def withLimitPerNode(o: Int) = copy(notInTaskLimitPerNode = Some(o))

  def withoutPathToMap: Map[String, String] = Map() ++
    (orderIdQuery.orderId map { o ⇒ OrderIdName → o.string }) ++
    (isDistributed map { o ⇒ IsDistributedName → o.toString }) ++
    (isSuspended map { o ⇒ IsSuspendedName → o.toString }) ++
    (isSetback map { o ⇒ IsSetbackName → o.toString }) ++
    (isBlacklisted map { o ⇒ IsBlacklistedName → o.toString}) ++
    (isOrderSourceType map ( o ⇒ IsOrderSourceTypeName → (o mkString ","))) ++
    (notInTaskLimitPerNode map { o ⇒ NotInTaskLimitPerNode → o.toString })
}

object OrderQuery {
  val All = OrderQuery()

  val OrderIdName = "orderId"
  val IsDistributedName = "isDistributed"
  val IsSuspendedName = "isSuspended"
  val IsSetbackName = "isSetback"
  val IsBlacklistedName = "isBlacklisted"
  val IsOrderSourceTypeName = "isOrderSourceType"
  val NotInTaskLimitPerNode = "notInTaskLimitPerNode"
}

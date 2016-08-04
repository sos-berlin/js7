package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.order.OrderSourceType
import com.sos.scheduler.engine.data.queries.OrderQuery._
import scala.collection.JavaConversions._

/**
  * @author Joacim Zschimmer
  */
final case class OrderQuery(
  jobChainPathQuery: PathQuery = PathQuery.All,
  isDistributed: Option[Boolean] = None,
  isSuspended: Option[Boolean] = None,
  isSetback: Option[Boolean] = None,
  isBlacklisted: Option[Boolean] = None,
  isOrderSourceType: Option[Set[OrderSourceType]] = None,
  notInTaskLimitPerNode: Option[Int] = None)
extends OnlyOrderQuery with JobChainQuery {

  for (limit ← notInTaskLimitPerNode) require(limit >= 0, s"Invalid notInTaskLimitPerNode=$notInTaskLimitPerNode")

  def withJobChainPathQuery(q: PathQuery) = copy(jobChainPathQuery = q)
  def withIsDistributed(o: Boolean) = copy(isDistributed = Some(o))
  def withIsSuspended(o: Boolean) = copy(isSuspended = Some(o))
  def withIsSetback(o: Boolean) = copy(isSetback = Some(o))
  def withIsBlacklisted(o: Boolean) = copy(isBlacklisted = Some(o))
  def withOrderSourceTypes(o: java.util.List[OrderSourceType]) = copy(isOrderSourceType = Some(o.toSet))
  def withLimitPerNode(o: Int) = copy(notInTaskLimitPerNode = Some(o))

  def withoutPathToMap: Map[String, String] = Map() ++
    (isSuspended map { o ⇒ SuspendedName → o.toString }) ++
    (isSetback map { o ⇒ SetbackName → o.toString }) ++
    (isBlacklisted map { o ⇒ BlacklistedName → o.toString}) ++
    (isOrderSourceType map ( o ⇒ SourceTypeName → (o mkString ","))) ++
    (isDistributed map { o ⇒ DistributedName → o.toString }) ++
    (notInTaskLimitPerNode map { o ⇒ LimitPerNodeName → o.toString })
}

object OrderQuery {
  val All = OrderQuery()

  val SuspendedName = "suspended"
  val SetbackName = "setback"
  val BlacklistedName = "blacklisted"
  val SourceTypeName = "sourceType"
  val DistributedName = "distributed"
  val LimitPerNodeName = "notInTaskLimitPerNode"
}

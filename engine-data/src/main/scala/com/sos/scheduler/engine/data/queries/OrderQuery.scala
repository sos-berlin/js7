package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.order.OrderSourceType
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
  isOrderSourceType: Option[Set[OrderSourceType]] = None)
extends OnlyOrderQuery with JobChainQuery {

  def withJobChainPathQuery(q: PathQuery) = copy(jobChainPathQuery = q)
  def withIsDistributed(o: Boolean) = copy(isDistributed = Some(o))
  def withIsSuspended(o: Boolean) = copy(isSuspended = Some(o))
  def withIsSetback(o: Boolean) = copy(isSetback = Some(o))
  def withIsBlacklisted(o: Boolean) = copy(isBlacklisted = Some(o))
  def withOrderSourceTypes(o: java.util.List[OrderSourceType]) = copy(isOrderSourceType = Some(o.toSet))
}

object OrderQuery {
  val All = OrderQuery()
}

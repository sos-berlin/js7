package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.order.OrderQuery._
import scala.collection.JavaConversions._
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderQuery(
  jobChains: String = "/",
  isSuspended: Option[Boolean] = None,
  isSetback: Option[Boolean] = None,
  isBlacklisted: Option[Boolean] = None,
  isSourceType: Option[Set[OrderSourceType]] = None)
extends (QueryableOrder ⇒ Boolean) {

  require((jobChains startsWith "/") && !(jobChains contains "/.."), "Invalid jobChain pattern")

  def apply(o: QueryableOrder) =
    (isSuspended forall { _ == o.isSuspended }) &&
    (isSetback forall { _ == o.isSetback }) &&
    (isBlacklisted forall { _ == o.isBlacklisted }) &&
    (isSourceType forall { _ contains o.sourceType }) &&
    jobChainPathMatches(jobChains, o.orderKey.jobChainPath)

  // For Java:
  def withJobChains(jobChains: String) = copy(jobChains = jobChains)
  def withIsSuspended(o: Boolean) = copy(isSuspended = Some(o))
  def withIsSetback(o: Boolean) = copy(isSetback = Some(o))
  def withIsBlacklisted(o: Boolean) = copy(isBlacklisted = Some(o))
  def withSourceTypes(sourceTypes: java.util.List[OrderSourceType]) = copy(isSourceType = Some(sourceTypes.toSet))
}

object OrderQuery {
  val AllOrderSourceTypes = OrderSourceType.values.toSet
  val All = new OrderQuery()
  private implicit val OrderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat5(apply)

  private def jobChainPathMatches(pattern: String, jobChainPath: JobChainPath) =
    if (pattern endsWith "/") jobChainPath.string startsWith pattern
    else jobChainPath.string == pattern
}


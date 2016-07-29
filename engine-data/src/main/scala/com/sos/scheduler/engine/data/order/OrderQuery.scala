package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.jobchain.JobChainQuery
import scala.collection.JavaConversions._
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderQuery(
  jobChainQuery: JobChainQuery = JobChainQuery.All,
  isSuspended: Option[Boolean] = None,
  isSetback: Option[Boolean] = None,
  isBlacklisted: Option[Boolean] = None,
  isSourceType: Option[Set[OrderSourceType]] = None) {

  def matches(o: QueryableOrder) =
    (isSuspended forall { _ == o.isSuspended }) &&
    (isSetback forall { _ == o.isSetback }) &&
    (isBlacklisted forall { _ == o.isBlacklisted }) &&
    (isSourceType forall { _ contains o.sourceType }) &&
    (jobChainQuery.matchesAll || jobChainQuery.matches(o.orderKey.jobChainPath))

  // For Java:
  def withJobChainQuery(o: JobChainQuery) = copy(jobChainQuery = o)
  def withIsSuspended(o: Boolean) = copy(isSuspended = Some(o))
  def withIsSetback(o: Boolean) = copy(isSetback = Some(o))
  def withIsBlacklisted(o: Boolean) = copy(isBlacklisted = Some(o))
  def withSourceTypes(o: java.util.List[OrderSourceType]) = copy(isSourceType = Some(o.toSet))

  def folderPath = jobChainQuery.folderPath
}

object OrderQuery {
  val AllOrderSourceTypes = OrderSourceType.values.toSet
  val All = new OrderQuery()
  private implicit val OrderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat5(apply)
}

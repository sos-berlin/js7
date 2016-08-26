package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.data.order.{OrderId, OrderKey, OrderSourceType}
import com.sos.scheduler.engine.data.queries.OrderQuery._
import scala.collection.JavaConversions._
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final case class OrderQuery(
  jobChainPathQuery: PathQuery = PathQuery.All,
  orderId: Option[OrderId] = None,
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
    orderId = Some(orderKey.id))
  def withIsDistributed(o: Boolean) = copy(isDistributed = Some(o))
  def withIsSuspended(o: Boolean) = copy(isSuspended = Some(o))
  def withIsSetback(o: Boolean) = copy(isSetback = Some(o))
  def withIsBlacklisted(o: Boolean) = copy(isBlacklisted = Some(o))
  def withOrderSourceTypes(o: java.util.List[OrderSourceType]) = copy(isOrderSourceType = Some(o.toSet))
  def withLimitPerNode(o: Int) = copy(notInTaskLimitPerNode = Some(o))

  def withoutPathToMap: Map[String, String] = Map() ++
    (orderId map { o ⇒ OrderIdName → o.string }) ++
    (isDistributed map { o ⇒ IsDistributedName → o.toString }) ++
    (isSuspended map { o ⇒ IsSuspendedName → o.toString }) ++
    (isSetback map { o ⇒ IsSetbackName → o.toString }) ++
    (isBlacklisted map { o ⇒ IsBlacklistedName → o.toString}) ++
    (isOrderSourceType map ( o ⇒ IsOrderSourceTypeName → (o mkString ","))) ++
    (notInTaskLimitPerNode map { o ⇒ NotInTaskLimitPerNode → o.toString })
}

object OrderQuery {
  val All = OrderQuery()

  private val PathName = "path"
  val OrderIdName = "orderId"
  val IsDistributedName = "isDistributed"
  val IsSuspendedName = "isSuspended"
  val IsSetbackName = "isSetback"
  val IsBlacklistedName = "isBlacklisted"
  val IsOrderSourceTypeName = "isOrderSourceType"
  val NotInTaskLimitPerNode = "notInTaskLimitPerNode"

  implicit val OrderQueryJsonFormat = new RootJsonFormat[OrderQuery] {
    private implicit def orderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat

    def write(q: OrderQuery) = JsObject((
      (q.jobChainPathQuery != PathQuery.All option (PathName → JsString(q.jobChainPathQuery.patternString))) ++
        (q.orderId map { o ⇒ OrderIdName → JsString(o.string) }) ++
        (q.isDistributed map { o ⇒ IsDistributedName → JsBoolean(o) }) ++
        (q.isSuspended map { o ⇒ IsSuspendedName → JsBoolean(o) }) ++
        (q.isSetback map { o ⇒ IsSetbackName → JsBoolean(o) }) ++
        (q.isBlacklisted map { o ⇒ IsBlacklistedName → JsBoolean(o) }) ++
        (q.isOrderSourceType map { o ⇒ IsOrderSourceTypeName → o.toJson }) ++
        (q.notInTaskLimitPerNode map { o ⇒ NotInTaskLimitPerNode → JsNumber(o) })).toMap)

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      OrderQuery(
        jobChainPathQuery = fields.get(PathName) match {
          case Some(path) ⇒ PathQuery(path.asInstanceOf[JsString].value)
          case None ⇒ PathQuery.All
        },
        orderId               = fields.get(OrderIdName          ) map { o ⇒ OrderId(o.asInstanceOf[JsString].value) },
        isDistributed         = fields.get(IsDistributedName    ) map { _.asInstanceOf[JsBoolean].value },
        isSuspended           = fields.get(IsSuspendedName      ) map { _.asInstanceOf[JsBoolean].value },
        isSetback             = fields.get(IsSetbackName        ) map { _.asInstanceOf[JsBoolean].value },
        isBlacklisted         = fields.get(IsBlacklistedName    ) map { _.asInstanceOf[JsBoolean].value },
        isOrderSourceType     = fields.get(IsOrderSourceTypeName) map { _.convertTo[Set[OrderSourceType] ]},
        notInTaskLimitPerNode = fields.get(NotInTaskLimitPerNode) map { _.asInstanceOf[JsNumber].value.toIntExact })
    }
  }
}

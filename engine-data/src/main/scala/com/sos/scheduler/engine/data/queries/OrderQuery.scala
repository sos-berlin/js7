package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.order.{OrderId, OrderKey, OrderProcessingState, OrderSourceType}
import com.sos.scheduler.engine.data.queries.OrderQuery._
import scala.reflect.ClassTag
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final case class OrderQuery(
  jobChainPathQuery: PathQuery = PathQuery.All,
  orderIds: Option[Set[OrderId]] = None,
  jobPaths: Option[Set[JobPath]] = None,
  isDistributed: Option[Boolean] = None,
  isSuspended: Option[Boolean] = None,
  isSetback: Option[Boolean] = None,
  isBlacklisted: Option[Boolean] = None,
  isOrderSourceType: Option[Set[OrderSourceType]] = None,
  isOrderProcessingState: Option[Set[Class[_ <: OrderProcessingState]]] = None,
  orIsSuspended: Boolean = false,
  notInTaskLimitPerNode: Option[Int] = None)
extends OnlyOrderQuery with JobChainQuery {

  for (limit ← notInTaskLimitPerNode) require(limit >= 0, s"Invalid notInTaskLimitPerNode=$notInTaskLimitPerNode")

  def withJobChainPathQuery(q: PathQuery) = copy(jobChainPathQuery = q)

  def withOrderKey(orderKey: OrderKey) = copy(
    jobChainPathQuery = PathQuery(orderKey.jobChainPath),
    orderIds = Some(Set(orderKey.id)))

  def toUriPathAndMap: (String, Map[String, String]) =
    (jobChainPathQuery.patternString, withoutPathToMap)

  private def withoutPathToMap: Map[String, String] = Map() ++
    toNamedCommaSeparated(OrderIdsName, orderIds)(_.string) ++
    toNamedCommaSeparated(JobPathsName, jobPaths)(_.string) ++
    (isDistributed map { o ⇒ IsDistributedName → o.toString }) ++
    (isSuspended map { o ⇒ IsSuspendedName → o.toString }) ++
    (isSetback map { o ⇒ IsSetbackName → o.toString }) ++
    (isBlacklisted map { o ⇒ IsBlacklistedName → o.toString}) ++
    toNamedCommaSeparated(IsOrderSourceTypeName, isOrderSourceType)(_.toString) ++
    toNamedCommaSeparated(IsOrderProcessingStateName, isOrderProcessingState)(OrderProcessingState.typedJsonFormat.classToTypeName) ++
    (notInTaskLimitPerNode map { o ⇒ NotInTaskLimitPerNode → o.toString })

  def orderKeyOption: Option[OrderKey] =
    (jobChainPathQuery, orderIds) match {
      case (singlePath: PathQuery.SinglePath, Some(ids)) if ids.size == 1 ⇒
        Some(singlePath.as[JobChainPath] orderKey ids.head)
      case _ ⇒
        None
    }
}

object OrderQuery {
  val All = OrderQuery()

  private val PathName = "path"
  val OrderIdsName = "orderIds"
  val JobPathsName = "jobPaths"
  val IsDistributedName = "isDistributed"
  val IsSuspendedName = "isSuspended"
  val IsSetbackName = "isSetback"
  val IsBlacklistedName = "isBlacklisted"
  val IsOrderSourceTypeName = "isOrderSourceType"
  val IsOrderProcessingStateName = "isOrderProcessingState"
  val OrIsSuspendedName = "orIsSuspended"
  val NotInTaskLimitPerNode = "notInTaskLimitPerNode"

  implicit val OrderQueryJsonFormat = new RootJsonFormat[OrderQuery] {
    import OrderProcessingState.typedJsonFormat.classJsonFormat
    private implicit def orderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat

    def write(q: OrderQuery) = JsObject((
      (q.jobChainPathQuery != PathQuery.All option (PathName → JsString(q.jobChainPathQuery.patternString))) ++
        (q.orderIds map { o ⇒ OrderIdsName → o.toJson }) ++
        (q.jobPaths map { o ⇒ JobPathsName → o.toJson }) ++
        (q.isDistributed map { o ⇒ IsDistributedName → JsBoolean(o) }) ++
        (q.isSuspended map { o ⇒ IsSuspendedName → JsBoolean(o) }) ++
        (q.isSetback map { o ⇒ IsSetbackName → JsBoolean(o) }) ++
        (q.isBlacklisted map { o ⇒ IsBlacklistedName → JsBoolean(o) }) ++
        (q.isOrderSourceType map { o ⇒ IsOrderSourceTypeName → o.toJson }) ++
        (q.isOrderProcessingState map { o ⇒ IsOrderProcessingStateName → o.toJson }) ++
        (q.orIsSuspended option { OrIsSuspendedName → JsTrue }) ++
        (q.notInTaskLimitPerNode map { o ⇒ NotInTaskLimitPerNode → JsNumber(o) })).toMap)

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      OrderQuery(
        jobChainPathQuery = fields.get(PathName) match {
          case Some(path) ⇒ PathQuery[JobChainPath](path.asInstanceOf[JsString].value)
          case None ⇒ PathQuery.All
        },
        orderIds               = fields.get(OrderIdsName              ) map { _.convertTo[Set[OrderId]] },
        jobPaths               = fields.get(JobPathsName              ) map { _.convertTo[Set[JobPath]] },
        isDistributed          = fields.get(IsDistributedName         ) map { _.asInstanceOf[JsBoolean].value },
        isSuspended            = fields.get(IsSuspendedName           ) map { _.asInstanceOf[JsBoolean].value },
        isSetback              = fields.get(IsSetbackName             ) map { _.asInstanceOf[JsBoolean].value },
        isBlacklisted          = fields.get(IsBlacklistedName         ) map { _.asInstanceOf[JsBoolean].value },
        isOrderSourceType      = fields.get(IsOrderSourceTypeName     ) map { _.convertTo[Set[OrderSourceType]] },
        isOrderProcessingState = fields.get(IsOrderProcessingStateName) map { _.convertTo[Set[Class[_ <: OrderProcessingState]]] },
        orIsSuspended          = fields.get(OrIsSuspendedName         ) map { _.convertTo[Boolean] } getOrElse false,
        notInTaskLimitPerNode  = fields.get(NotInTaskLimitPerNode     ) map { _.asInstanceOf[JsNumber].value.toIntExact })
    }
  }

  private def toNamedCommaSeparated[A: ClassTag](name: String, elementsOption: Option[Iterable[A]])(toString: A ⇒ String): Option[(String, String)] =
    for (elements ← elementsOption) yield name → toCommaSeparated(toString)(elements)

  private def toCommaSeparated[A: ClassTag](toString: A ⇒ String)(elements: Iterable[A]): String =
    (for (o ← elements) yield {
      val string = toString(o)
      require(!string.contains(','), s"For this serialization, a ${implicitClass[A]} may not contain a comma ',': '$o'")
      string
    }) mkString ","
}

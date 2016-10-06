package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.base.convert.ConvertiblePartialFunctions._
import com.sos.scheduler.engine.base.serial.PathAndParameterSerializable
import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.order.{OrderId, OrderKey, OrderProcessingState, OrderSourceType}
import com.sos.scheduler.engine.data.queries.CommaSeparated._
import com.sos.scheduler.engine.data.queries.OrderQuery._
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final case class OrderQuery(
  nodeQuery: JobChainNodeQuery = JobChainNodeQuery.All,
  orderIds: Option[Set[OrderId]] = None,
  isSuspended: Option[Boolean] = None,
  isSetback: Option[Boolean] = None,
  isBlacklisted: Option[Boolean] = None,
  isOrderSourceType: Option[Set[OrderSourceType]] = None,
  isOrderProcessingState: Option[Set[Class[_ <: OrderProcessingState]]] = None,
  orIsSuspended: Boolean = false,
  notInTaskLimitPerNode: Option[Int] = None)
extends OnlyOrderQuery {

  for (limit ← notInTaskLimitPerNode) require(limit >= 0, s"Invalid notInTaskLimitPerNode=$notInTaskLimitPerNode")

  def nodeIds = nodeQuery.nodeIds

  def jobPaths = nodeQuery.jobPaths

  def jobChainQuery: JobChainQuery = nodeQuery.jobChainQuery

  def withOrderKey(orderKey: OrderKey) = copy(
    nodeQuery.copy(jobChainQuery = nodeQuery.jobChainQuery.copy(pathQuery = PathQuery(orderKey.jobChainPath))),
    orderIds = Some(Set(orderKey.id)))

  def withPathQuery(q: PathQuery) = withJobChainQuery(jobChainQuery.copy(pathQuery = q))

  def withJobChainQuery(q: JobChainQuery) = copy(nodeQuery = nodeQuery.copy(jobChainQuery = q))

  def toPathAndParameters: (String, Map[String, String]) = {
    val (path, jobChainParameters) = nodeQuery.toPathAndParameters
    val parameters = jobChainParameters ++
      toNamedCommaSeparated(OrderIdsName, orderIds)(_.string) ++
      (isSuspended map { o ⇒ IsSuspendedName → o.toString }) ++
      (isSetback map { o ⇒ IsSetbackName → o.toString }) ++
      (isBlacklisted map { o ⇒ IsBlacklistedName → o.toString}) ++
      toNamedCommaSeparated(IsOrderSourceTypeName, isOrderSourceType)(_.toString) ++
      toNamedCommaSeparated(IsOrderProcessingStateName, isOrderProcessingState)(OrderProcessingState.typedJsonFormat.classToTypeName) ++
      (orIsSuspended option ("orIsSuspended" → "true")) ++
      (notInTaskLimitPerNode map { o ⇒ NotInTaskLimitPerNode → o.toString })
    (path, parameters)
  }

  def orderKeyOption: Option[OrderKey] =
    (nodeQuery.jobChainQuery.pathQuery, orderIds) match {
      case (singlePath: PathQuery.SinglePath, Some(ids)) if ids.size == 1 ⇒
        Some(singlePath.as[JobChainPath] orderKey ids.head)
      case _ ⇒
        None
    }
}

object OrderQuery {
  val All = OrderQuery()

  val OrderIdsName = "orderIds"
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

    def write(q: OrderQuery) = JsObject(
      q.nodeQuery.toJson.asJsObject.fields ++
      (q.orderIds map { o ⇒ OrderIdsName → o.toJson }) ++
      (q.isSuspended map { o ⇒ IsSuspendedName → JsBoolean(o) }) ++
      (q.isSetback map { o ⇒ IsSetbackName → JsBoolean(o) }) ++
      (q.isBlacklisted map { o ⇒ IsBlacklistedName → JsBoolean(o) }) ++
      (q.isOrderSourceType map { o ⇒ IsOrderSourceTypeName → o.toJson }) ++
      (q.isOrderProcessingState map { o ⇒ IsOrderProcessingStateName → o.toJson }) ++
      (q.orIsSuspended option { OrIsSuspendedName → JsTrue }) ++
      (q.notInTaskLimitPerNode map { o ⇒ NotInTaskLimitPerNode → JsNumber(o) }))

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      OrderQuery(
        nodeQuery      = json.convertTo[JobChainNodeQuery],
        orderIds               = fields.get(OrderIdsName              ) map { _.convertTo[Set[OrderId]] },
        isSuspended            = fields.get(IsSuspendedName           ) map { _.asInstanceOf[JsBoolean].value },
        isSetback              = fields.get(IsSetbackName             ) map { _.asInstanceOf[JsBoolean].value },
        isBlacklisted          = fields.get(IsBlacklistedName         ) map { _.asInstanceOf[JsBoolean].value },
        isOrderSourceType      = fields.get(IsOrderSourceTypeName     ) map { _.convertTo[Set[OrderSourceType]] },
        isOrderProcessingState = fields.get(IsOrderProcessingStateName) map { _.convertTo[Set[Class[_ <: OrderProcessingState]]] },
        orIsSuspended          = fields.get(OrIsSuspendedName         ) map { _.convertTo[Boolean] } getOrElse false,
        notInTaskLimitPerNode  = fields.get(NotInTaskLimitPerNode     ) map { _.asInstanceOf[JsNumber].value.toIntExact })
    }
  }

  implicit object pathAndParameterSerializable extends PathAndParameterSerializable[OrderQuery] {
    def toPathAndParameters(q: OrderQuery) = q.toPathAndParameters

    def fromPathAndParameters(pathAndParameters: (String, Map[String, String])) = {
      val (path, parameters) = pathAndParameters
      OrderQuery(
        nodeQuery = PathAndParameterSerializable.fromPathAndParameters[JobChainNodeQuery]((path, parameters)),
        orderIds = parameters.optionAs(OrderIdsName)(commaSplittedAsSet(OrderId.apply)),
        isSuspended = parameters.optionAs[Boolean](IsSuspendedName),
        isSetback = parameters.optionAs[Boolean](IsSetbackName),
        isBlacklisted = parameters.optionAs[Boolean](IsBlacklistedName),
        isOrderSourceType = parameters.optionAs(IsOrderSourceTypeName)(commaSplittedAsSet(OrderSourceType.valueOf)),
        isOrderProcessingState = parameters.optionAs(IsOrderProcessingStateName)(commaSplittedAsSet(OrderProcessingState.typedJsonFormat.typeNameToClass)),
        notInTaskLimitPerNode = parameters.optionAs[Int](NotInTaskLimitPerNode),
        orIsSuspended = parameters.as[Boolean](OrIsSuspendedName, false))
    }
  }
}

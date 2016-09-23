package com.sos.scheduler.engine.data.queries

import com.google.common.base.Splitter
import com.sos.scheduler.engine.base.convert.As
import com.sos.scheduler.engine.base.convert.ConvertiblePartialFunctions._
import com.sos.scheduler.engine.base.serial.PathAndParameterSerializable
import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.order.{OrderId, OrderKey, OrderProcessingState, OrderSourceType}
import com.sos.scheduler.engine.data.queries.OrderQuery._
import scala.collection.JavaConversions._
import scala.reflect.ClassTag
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final case class OrderQuery(
  jobChainQuery: JobChainQuery = JobChainQuery.All,
  orderIds: Option[Set[OrderId]] = None,
  jobPaths: Option[Set[JobPath]] = None,
  isSuspended: Option[Boolean] = None,
  isSetback: Option[Boolean] = None,
  isBlacklisted: Option[Boolean] = None,
  isOrderSourceType: Option[Set[OrderSourceType]] = None,
  isOrderProcessingState: Option[Set[Class[_ <: OrderProcessingState]]] = None,
  orIsSuspended: Boolean = false,
  notInTaskLimitPerNode: Option[Int] = None)
extends OnlyOrderQuery {

  for (limit ← notInTaskLimitPerNode) require(limit >= 0, s"Invalid notInTaskLimitPerNode=$notInTaskLimitPerNode")

  def withOrderKey(orderKey: OrderKey) = copy(
    jobChainQuery.copy(pathQuery = PathQuery(orderKey.jobChainPath)),
    orderIds = Some(Set(orderKey.id)))

  def toPathAndParameters: (String, Map[String, String]) = {
    val (path, jobChainParameters) = jobChainQuery.toPathAndParameters
    val parameters = jobChainParameters ++
      toNamedCommaSeparated(OrderIdsName, orderIds)(_.string) ++
      toNamedCommaSeparated(JobPathsName, jobPaths)(_.string) ++
      (isSuspended map { o ⇒ IsSuspendedName → o.toString }) ++
      (isSetback map { o ⇒ IsSetbackName → o.toString }) ++
      (isBlacklisted map { o ⇒ IsBlacklistedName → o.toString}) ++
      toNamedCommaSeparated(IsOrderSourceTypeName, isOrderSourceType)(_.toString) ++
      toNamedCommaSeparated(IsOrderProcessingStateName, isOrderProcessingState)(OrderProcessingState.typedJsonFormat.classToTypeName) ++
      (notInTaskLimitPerNode map { o ⇒ NotInTaskLimitPerNode → o.toString })
    (path, parameters)
  }

  def orderKeyOption: Option[OrderKey] =
    (jobChainQuery.pathQuery, orderIds) match {
      case (singlePath: PathQuery.SinglePath, Some(ids)) if ids.size == 1 ⇒
        Some(singlePath.as[JobChainPath] orderKey ids.head)
      case _ ⇒
        None
    }
}

object OrderQuery {
  val All = OrderQuery()

  val OrderIdsName = "orderIds"
  val JobPathsName = "jobPaths"
  val IsSuspendedName = "isSuspended"
  val IsSetbackName = "isSetback"
  val IsBlacklistedName = "isBlacklisted"
  val IsOrderSourceTypeName = "isOrderSourceType"
  val IsOrderProcessingStateName = "isOrderProcessingState"
  val OrIsSuspendedName = "orIsSuspended"
  val NotInTaskLimitPerNode = "notInTaskLimitPerNode"

  private val CommaSplitter = Splitter.on(',')

  private[queries] def commaSplittedAsSet[A](to: String ⇒ A) = As[String, Set[A]] {
    case "" ⇒ Set()
    case o ⇒ (CommaSplitter split o map to).toSet
  }

  implicit val OrderQueryJsonFormat = new RootJsonFormat[OrderQuery] {
    import OrderProcessingState.typedJsonFormat.classJsonFormat
    private implicit def orderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat

    def write(q: OrderQuery) = JsObject(
      q.jobChainQuery.toJson.asJsObject.fields ++
      (q.orderIds map { o ⇒ OrderIdsName → o.toJson }) ++
      (q.jobPaths map { o ⇒ JobPathsName → o.toJson }) ++
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
        jobChainQuery          = json.convertTo[JobChainQuery],
        orderIds               = fields.get(OrderIdsName              ) map { _.convertTo[Set[OrderId]] },
        jobPaths               = fields.get(JobPathsName              ) map { _.convertTo[Set[JobPath]] },
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
        jobChainQuery = PathAndParameterSerializable.fromPathAndParameters[JobChainQuery]((path, parameters)),
        orderIds = parameters.optionAs(OrderIdsName)(commaSplittedAsSet(OrderId.apply)),
        jobPaths = parameters.optionAs(JobPathsName)(commaSplittedAsSet(JobPath.apply)),
        isSuspended = parameters.optionAs[Boolean](IsSuspendedName),
        isSetback = parameters.optionAs[Boolean](IsSetbackName),
        isBlacklisted = parameters.optionAs[Boolean](IsBlacklistedName),
        isOrderSourceType = parameters.optionAs(IsOrderSourceTypeName)(commaSplittedAsSet(OrderSourceType.valueOf)),
        isOrderProcessingState = parameters.optionAs(IsOrderProcessingStateName)(commaSplittedAsSet(OrderProcessingState.typedJsonFormat.typeNameToClass)),
        notInTaskLimitPerNode = parameters.optionAs[Int](NotInTaskLimitPerNode),
        orIsSuspended = parameters.as[Boolean](OrIsSuspendedName, false))
    }
  }

  private def toNamedCommaSeparated[A: ClassTag](name: String, elementsOption: Option[Iterable[A]])(toString: A ⇒ String): Option[(String, String)] =
    for (elements ← elementsOption) yield name → toCommaSeparated(toString)(elements)

  private def toCommaSeparated[A: ClassTag](toString: A ⇒ String)(elements: Iterable[A]): String =
    (for (o ← elements) yield {
      val string = toString(o)
      require(!string.contains(','), s"For this serialization, a ${implicitClass[A]} must not contain a comma ',': '$o'")
      string
    }) mkString ","
}

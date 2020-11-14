package js7.proxy.javaapi.data.order

import io.circe.{Decoder, Encoder}
import io.vavr.control.{Either => VEither}
import java.time.Instant
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.time.Timestamp
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.Value
import js7.data.workflow.WorkflowPath
import js7.proxy.javaapi.data.common.JJsonable
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

@javaApi
final case class JFreshOrder(asScala: FreshOrder)
extends JJsonable[JFreshOrder]
{
  protected type AsScala = FreshOrder

  protected def companion = JFreshOrder

  def id: OrderId =
    asScala.id
}

@javaApi
object JFreshOrder extends JJsonable.Companion[JFreshOrder]
{
  @throws[RuntimeException]("on invalid syntax")
  def of(id: OrderId, workflowPath: WorkflowPath): JFreshOrder =
    JFreshOrder(FreshOrder(id, workflowPath, None, Map.empty))

  @throws[RuntimeException]("on invalid syntax")
  def of(
    id: OrderId,
    workflowPath: WorkflowPath,
    scheduledFor: java.util.Optional[Instant],
    arguments: java.util.Map[String, Value] = java.util.Collections.emptyMap())
  : JFreshOrder =
    JFreshOrder(FreshOrder(id, workflowPath,
      scheduledFor.toScala.map(o => Timestamp.ofEpochMilli(o.toEpochMilli)),
      arguments.asScala.toMap))

  override def fromJson(jsonString: String): VEither[Problem, JFreshOrder] =
    super.fromJson(jsonString)

  def jsonEncoder = implicitly[Encoder[FreshOrder]]

  def jsonDecoder = implicitly[Decoder[FreshOrder]]
}

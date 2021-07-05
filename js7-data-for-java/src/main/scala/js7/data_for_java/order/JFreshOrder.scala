package js7.data_for_java.order

import io.vavr.control.{Either => VEither}
import java.time.Instant
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.time.Timestamp
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.Value
import js7.data.workflow.WorkflowPath
import js7.data_for_java.common.JJsonable
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

@javaApi
final case class JFreshOrder(asScala: FreshOrder)
extends JJsonable[JFreshOrder]
{
  protected type AsScala = FreshOrder

  protected def companion = JFreshOrder

  @Nonnull
  def id: OrderId =
    asScala.id
}

@javaApi
object JFreshOrder extends JJsonable.Companion[JFreshOrder]
{
  @Nonnull
  @throws[RuntimeException]("on invalid syntax")
  def of(
    @Nonnull id: OrderId,
    @Nonnull workflowPath: WorkflowPath)
  : JFreshOrder =
    JFreshOrder(FreshOrder(id, workflowPath))

  @Nonnull
  @throws[RuntimeException]("on invalid syntax")
  def of(
    @Nonnull id: OrderId,
    @Nonnull workflowPath: WorkflowPath,
    @Nonnull scheduledFor: java.util.Optional[Instant],
    @Nonnull arguments: java.util.Map[String, Value] = java.util.Collections.emptyMap())
  : JFreshOrder =
    JFreshOrder(FreshOrder(id, workflowPath,
      arguments.asScala.toMap,
      scheduledFor.toScala.map(o => Timestamp.ofEpochMilli(o.toEpochMilli))))

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JFreshOrder] =
    super.fromJson(jsonString)

  protected def jsonEncoder = FreshOrder.jsonEncoder
  protected def jsonDecoder = FreshOrder.jsonDecoder
}

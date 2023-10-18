package js7.data_for_java.order

import io.vavr.control.Either as VEither
import java.time.Instant
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.time.Timestamp
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.Value
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.BranchPath
import js7.data_for_java.common.JJsonable
import js7.data_for_java.workflow.position.{JBranchPath, JPositionOrLabel}
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

@javaApi
final case class JFreshOrder(asScala: FreshOrder)
extends JJsonable[JFreshOrder]:

  type AsScala = FreshOrder

  protected def companion = JFreshOrder

  @Nonnull
  def id: OrderId =
    asScala.id

  @Nonnull
  def innerBlock: java.util.List[BranchPath.Segment] =
    asScala.innerBlock.asJava

  @Nonnull
  def startPosition: Optional[JPositionOrLabel] =
    asScala.startPosition.map(JPositionOrLabel(_)).toJava

  @Nonnull
  def stopPositions: java.util.Set[JPositionOrLabel] =
    asScala.stopPositions.map(JPositionOrLabel(_)).asJava


@javaApi
object JFreshOrder extends JJsonable.Companion[JFreshOrder]:
  type AsScala = FreshOrder

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
    @Nonnull arguments: java.util.Map[String, Value])
  : JFreshOrder =
    of(id, workflowPath, scheduledFor, arguments, deleteWhenTerminated = false)

  @Nonnull
  @throws[RuntimeException]("on invalid syntax")
  def of(
    @Nonnull id: OrderId,
    @Nonnull workflowPath: WorkflowPath,
    @Nonnull scheduledFor: java.util.Optional[Instant],
    @Nonnull arguments: java.util.Map[String, Value],
    @Nonnull deleteWhenTerminated: Boolean)
  : JFreshOrder =
    JFreshOrder(FreshOrder(
      id,
      workflowPath,
      arguments.asScala.toMap,
      scheduledFor.toScala.map(o => Timestamp.ofEpochMilli(o.toEpochMilli)),
      deleteWhenTerminated = deleteWhenTerminated))

  @Nonnull
  @throws[RuntimeException]("on invalid syntax")
  def of(
    @Nonnull id: OrderId,
    @Nonnull workflowPath: WorkflowPath,
    @Nonnull scheduledFor: java.util.Optional[Instant],
    @Nonnull arguments: java.util.Map[String, Value],
    @Nonnull deleteWhenTerminated: Boolean,
    @Nonnull startPosition: Optional[JPositionOrLabel],
    @Nonnull stopPositions: java.util.Set[JPositionOrLabel])
  : JFreshOrder =
    JFreshOrder(FreshOrder(
      id,
      workflowPath,
      arguments.asScala.toMap,
      scheduledFor.toScala.map(o => Timestamp.ofEpochMilli(o.toEpochMilli)),
      deleteWhenTerminated = deleteWhenTerminated,
      startPosition = startPosition.toScala.map(_.asScala),
      stopPositions = stopPositions.asScala.map(_.asScala).toSet))

  @Nonnull
  @throws[RuntimeException]("on invalid syntax")
  def of(
    @Nonnull id: OrderId,
    @Nonnull workflowPath: WorkflowPath,
    @Nonnull scheduledFor: java.util.Optional[Instant],
    @Nonnull arguments: java.util.Map[String, Value],
    @Nonnull deleteWhenTerminated: Boolean,
    @Nonnull forceJobAdmission: Boolean,
    @Nonnull startPosition: Optional[JPositionOrLabel],
    @Nonnull stopPositions: java.util.Set[JPositionOrLabel])
  : JFreshOrder =
    JFreshOrder(FreshOrder(
      id,
      workflowPath,
      arguments.asScala.toMap,
      scheduledFor.toScala.map(o => Timestamp.ofEpochMilli(o.toEpochMilli)),
      deleteWhenTerminated = deleteWhenTerminated,
      forceJobAdmission = forceJobAdmission,
      startPosition = startPosition.toScala.map(_.asScala),
      stopPositions = stopPositions.asScala.map(_.asScala).toSet))

  @Nonnull
  @throws[RuntimeException]("on invalid syntax")
  def of(
    @Nonnull id: OrderId,
    @Nonnull workflowPath: WorkflowPath,
    @Nonnull scheduledFor: java.util.Optional[Instant],
    @Nonnull arguments: java.util.Map[String, Value],
    @Nonnull deleteWhenTerminated: Boolean,
    @Nonnull forceJobAdmission: Boolean,
    @Nonnull innerBlock: JBranchPath,
    @Nonnull startPosition: Optional[JPositionOrLabel],
    @Nonnull stopPositions: java.util.Set[JPositionOrLabel])
  : JFreshOrder =
    JFreshOrder(FreshOrder(
      id,
      workflowPath,
      arguments.asScala.toMap,
      scheduledFor.toScala.map(o => Timestamp.ofEpochMilli(o.toEpochMilli)),
      deleteWhenTerminated = deleteWhenTerminated,
      forceJobAdmission = forceJobAdmission,
      innerBlock = innerBlock.asScala,
      startPosition = startPosition.toScala.map(_.asScala),
      stopPositions = stopPositions.asScala.map(_.asScala).toSet))

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JFreshOrder] =
    super.fromJson(jsonString)

  protected def jsonEncoder = FreshOrder.jsonEncoder
  protected def jsonDecoder = FreshOrder.jsonDecoder

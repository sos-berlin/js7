package js7.data_for_java.workflow.position

import io.circe.{Decoder, Encoder}
import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.workflow.position.WorkflowPosition
import js7.data_for_java.common.JJsonable
import js7.data_for_java.workflow.JWorkflowId

final case class JWorkflowPosition(asScala: WorkflowPosition)
extends JJsonable [JWorkflowPosition]:
  type AsScala = WorkflowPosition
  protected def companion = JWorkflowPosition

  @Nonnull
  def workflowId: JWorkflowId =
    JWorkflowId(asScala.workflowId)

  @Nonnull
  def position: JPosition =
    JPosition(asScala.position)

object JWorkflowPosition extends JJsonable.Companion[JWorkflowPosition]:
  type AsScala = WorkflowPosition

  @Nonnull
  def of(
    @Nonnull workflowId: JWorkflowId,
    @Nonnull position: JPosition)
  : JWorkflowPosition =
    new JWorkflowPosition(WorkflowPosition(workflowId.asScala, position.asScala))

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JWorkflowPosition] =
    super.fromJson(jsonString)

  protected def jsonEncoder: Encoder.AsObject[WorkflowPosition] =
    WorkflowPosition.jsonCodec

  protected def jsonDecoder: Decoder[WorkflowPosition] =
    WorkflowPosition.jsonCodec

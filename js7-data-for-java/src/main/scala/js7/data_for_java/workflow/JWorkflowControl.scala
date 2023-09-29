package js7.data_for_java.workflow

import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.workflow.{WorkflowControl, WorkflowControlPath}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedVersionedItem
import js7.data_for_java.workflow.position.JPosition
import scala.jdk.CollectionConverters.*

@javaApi
final case class JWorkflowControl(asScala: WorkflowControl)
extends JUnsignedVersionedItem[JWorkflowControl, WorkflowControlPath]:
  type AsScala = WorkflowControl

  def companion = JWorkflowControl

  @Nonnull
  def id: JWorkflowControlId =
    JWorkflowControlId(asScala.id)

  @Nonnull
  def breakpoints: java.util.Set[JPosition] =
    asScala.breakpoints.map(JPosition(_)).asJava

@javaApi
object JWorkflowControl extends JJsonable.Companion[JWorkflowControl]:
  type AsScala = WorkflowControl

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JWorkflowControl] =
    super.fromJson(jsonString)

  protected def jsonEncoder = WorkflowControl.jsonEncoder
  protected def jsonDecoder = WorkflowControl.jsonDecoder

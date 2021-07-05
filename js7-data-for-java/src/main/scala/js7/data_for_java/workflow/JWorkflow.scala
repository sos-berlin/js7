package js7.data_for_java.workflow

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JVersionedItem
import js7.data_for_java.vavr.VavrConverters._
import js7.data_for_java.workflow.position.JPosition
import scala.jdk.CollectionConverters._

@javaApi
final case class JWorkflow(asScala: Workflow)
extends JVersionedItem[JWorkflow, WorkflowPath]
{
  protected type AsScala = Workflow

  def companion = JWorkflow

  @Nonnull
  def id = JWorkflowId(asScala.id)

  @Nonnull
  def checkedJobName(position: JPosition): VEither[Problem, WorkflowJob.Name] =
    asScala.checkedExecute(position.asScala)
      .flatMap {
        case named: Execute.Named => Right(named.name)
        case _ => Left(Problem(s"Job at position $position does not have a name"))
      }
      .toVavr

  @Nonnull
  def withPositions = JWorkflow(asScala.withPositions(Nil))

  /** Positions to which a given order can be moved. */
  @Nonnull
  def reachablePositions(from: JPosition): java.util.List[JPosition] =
    asScala.reachablePositions(from.asScala)
      .view.map(JPosition(_))
      .toVector.asJava
}

@javaApi
object JWorkflow extends JJsonable.Companion[JWorkflow]
{
  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JWorkflow] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Workflow.jsonEncoder
  protected def jsonDecoder = Workflow.jsonDecoder
}

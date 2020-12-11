package js7.proxy.javaapi.data.workflow

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.javaapi.data.common.JJsonable
import js7.proxy.javaapi.data.common.VavrConverters._
import js7.proxy.javaapi.data.item.JVersionedItem
import js7.proxy.javaapi.data.workflow.position.JPosition

@javaApi
final case class JWorkflow(asScala: Workflow)
extends JVersionedItem[JWorkflow, WorkflowPath]
{
  protected type AsScala = Workflow

  def companion = JWorkflow

  def id = JWorkflowId(asScala.id)

  def checkedJobName(position: JPosition): VEither[Problem, WorkflowJob.Name] =
    asScala.checkedExecute(position.asScala)
      .flatMap {
        case named: Execute.Named => Right(named.name)
        case _ => Left(Problem(s"Job at position $position does not have a name"))
      }
      .toVavr

  def withPositions = JWorkflow(asScala.withPositions(Nil))
}

@javaApi
object JWorkflow extends JJsonable.Companion[JWorkflow]
{
  override def fromJson(jsonString: String): VEither[Problem, JWorkflow] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Workflow.jsonEncoder
  protected def jsonDecoder = Workflow.jsonDecoder
}

package js7.data.value.expression

import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Label

sealed trait PositionSearch

object PositionSearch
{
  final case class ByPrefix(name: String)
  extends PositionSearch

  final case class ByWorkflowJob(jobName: WorkflowJob.Name)
  extends PositionSearch
  {
    override def toString = s"LastExecutedJob(${jobName.string})"
  }

  final case class ByLabel(label: Label)
  extends PositionSearch
  {
    override def toString = s"ByLabel(${label.string})"
  }
}

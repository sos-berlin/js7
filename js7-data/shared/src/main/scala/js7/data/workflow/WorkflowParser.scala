package js7.data.workflow

import js7.base.problem.Checked

object WorkflowParser
{
  def parse(string: String): Checked[Workflow] =
    CatsWorkflowParser.parse(string)

  def parse(id: WorkflowId, string: String): Checked[Workflow] =
    CatsWorkflowParser.parse(id, string)
}

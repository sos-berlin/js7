package js7.data.workflow

import js7.base.problem.Checked
import js7.data.parser.UseFastparse

object WorkflowParser
{
  def parse(string: String): Checked[Workflow] =
    if (UseFastparse)
      FastparseWorkflowParser.parse(string)
    else
      CatsWorkflowParser.parse(string)

  def parse(id: WorkflowId, string: String): Checked[Workflow] =
    if (UseFastparse)
      FastparseWorkflowParser.parse(id, string)
    else
      CatsWorkflowParser.parse(id, string)
}

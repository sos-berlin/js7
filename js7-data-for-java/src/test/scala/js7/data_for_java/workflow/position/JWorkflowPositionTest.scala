package js7.data_for_java.workflow.position

import js7.base.test.OurTestSuite
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.BranchId.{Then, fork}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position

final class JWorkflowPositionTest extends OurTestSuite
{
  "JWorkflowPosition.toString" in {
    val workflowId = WorkflowPath("WORKFLOW") ~ "1.0"
    assert(JWorkflowPosition(workflowId /: Position(0)).toString == "WORKFLOW~1.0:0")

    assert(JWorkflowPosition(workflowId /: (Position(1) / Then % 2 / fork("🥕") % 3)).toString ==
      "WORKFLOW~1.0:1/then:2/fork+🥕:3")
  }
}

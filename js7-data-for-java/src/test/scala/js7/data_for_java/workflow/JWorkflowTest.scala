package js7.data_for_java.workflow

import java.util.Arrays.asList
import js7.base.test.OurTestSuite
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.If
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.data_for_java.workflow.position.JPosition
import scala.jdk.CollectionConverters.*

final class JWorkflowTest extends OurTestSuite:

  "reachablePositions" in:
    val workflow = JWorkflow(Workflow.of(WorkflowPath("WORKFLOW") ~ "1.0",
      If(expr("false")).Then:
        Workflow.empty
      .elseIf(expr("false")).Then:
        Workflow.empty
      .elseIf(expr("true")).Then:
        If(expr("false")).Then:
          Workflow.empty
        .Else:
          Workflow.empty
      .Else:
        Workflow.empty))

    assert:
      workflow.reachablePositions(JPosition(Position(0))) ==
        asList(
          JPosition(Position(0)),
          JPosition(Position(0) / "then" % 0),
          JPosition(Position(0) / "then+2" % 0),
          JPosition(Position(0) / "then+3" % 0),
          JPosition(Position(0) / "then+3" % 0 / "then" % 0),
          JPosition(Position(0) / "then+3" % 0 / "else" % 0),
          JPosition(Position(0) / "then+3" % 1),
          JPosition(Position(0) / "else" % 0),
          JPosition(Position(1)))

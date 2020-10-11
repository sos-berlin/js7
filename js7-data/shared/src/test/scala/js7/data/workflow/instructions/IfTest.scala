package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.data.agent.AgentName
import js7.data.expression.Expression._
import js7.data.job.ExecutablePath
import js7.data.source.SourcePos
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.BranchId.{Else, Then}
import js7.data.workflow.position.Position
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IfTest extends AnyFreeSpec
{
  private val if_ = If(
    GreaterOrEqual(LastReturnCode, NumericConstant(3)),
    thenWorkflow = Workflow.of(Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/THEN")))),
    elseWorkflow = Some(Workflow.of(Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/ELSE"))))),
    Some(SourcePos(1, 2)))

  "JSON" in {
    testJson[Instruction.Labeled](if_,
      json"""{
        "TYPE": "If",
        "predicate": "returnCode >= 3",
        "then": {
          "instructions": [
            {
              "TYPE": "Execute.Anonymous",
              "job": {
                "agentName": "AGENT",
                "executable": {
                  "TYPE": "ExecutablePath",
                  "path": "/THEN"
                },
                "taskLimit": 1
              }
            }
          ]
        },
        "else": {
          "instructions": [
            {
              "TYPE": "Execute.Anonymous",
              "job":  {
                "agentName": "AGENT",
                "executable": {
                  "TYPE": "ExecutablePath",
                  "path": "/ELSE"
                },
                "taskLimit": 1
              }
            }
          ]
        },
        "sourcePos": [ 1, 2 ]
      }""")
  }

  "workflow" in {
    assert(if_.workflow(Then) == Right(if_.thenWorkflow))
    assert(if_.workflow(Else) == Right(if_.elseWorkflow.get))
    assert(if_.workflow("A").isLeft)
  }

  "flattenedBranchToWorkflow" in {
    assert(if_.flattenedWorkflows(Position(7)) ==
      ((Position(7) / Then) -> if_.thenWorkflow) ::
      ((Position(7) / Else) -> if_.elseWorkflow.get) :: Nil)
  }

  "flattenedInstructions" in {
    assert(if_.flattenedInstructions(Position(7)) == Vector[(Position, Instruction.Labeled)](
      Position(7) / Then % 0 -> if_.thenWorkflow.instructions(0),
      Position(7) / Then % 1 -> ImplicitEnd(),
      Position(7) / Else % 0 -> if_.elseWorkflow.get.instructions(0),
      Position(7) / Else % 1 -> ImplicitEnd()))
  }
}

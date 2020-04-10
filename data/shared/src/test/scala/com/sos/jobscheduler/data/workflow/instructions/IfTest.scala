package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.expression.Expression._
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.BranchId.{Else, Then}
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IfTest extends AnyFreeSpec
{
  private val if_ = If(
    GreaterOrEqual(LastReturnCode, NumericConstant(3)),
    thenWorkflow = Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/THEN")))),
    elseWorkflow = Some(Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/ELSE"))))),
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
                "agentRefPath": "/AGENT",
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
                "agentRefPath": "/AGENT",
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

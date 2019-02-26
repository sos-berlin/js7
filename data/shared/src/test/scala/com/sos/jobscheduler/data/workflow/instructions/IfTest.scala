package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.workflow.instructions.If.{Else, Then}
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression._
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IfTest extends FreeSpec
{
  private val if_ = If(
    GreaterOrEqual(OrderReturnCode, NumericConstant(3)),
    thenWorkflow = Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/THEN")))),
    elseWorkflow = Some(Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/ELSE"))))))

  "JSON" in {
    testJson[Instruction.Labeled](if_,
      json"""{
        "TYPE": "If",
        "predicate": "returnCode >= 3",
        "then": {
          "instructions": [
            { "TYPE": "Execute.Anonymous", "job": { "agentRefPath": "/AGENT", "executablePath": "/THEN", "taskLimit": 1 }}
          ]
        },
        "else": {
          "instructions": [
            { "TYPE": "Execute.Anonymous", "job": { "agentRefPath": "/AGENT", "executablePath": "/ELSE", "taskLimit": 1 }}
          ]
        }
      }""")
  }

  "workflow" in {
    assert(if_.workflow(Then) == Valid(if_.thenWorkflow))
    assert(if_.workflow(Else) == Valid(if_.elseWorkflow.get))
    assert(if_.workflow("A").isInvalid)
  }

  "flattenedWorkflows" in {
    assert(if_.flattenedWorkflows(Position(7)) ==
      ((Position(7) / Then) → if_.thenWorkflow) ::
      ((Position(7) / Else) → if_.elseWorkflow.get) :: Nil)
  }

  "flattenedInstructions" in {
    assert(if_.flattenedInstructions(Position(7)) == Vector[(Position, Instruction.Labeled)](
      Position(7) / Then % 0 -> if_.thenWorkflow.instructions(0),
      Position(7) / Then % 1 -> ImplicitEnd,
      Position(7) / Else % 0 -> if_.elseWorkflow.get.instructions(0),
      Position(7) / Else % 1 -> ImplicitEnd))
  }
}

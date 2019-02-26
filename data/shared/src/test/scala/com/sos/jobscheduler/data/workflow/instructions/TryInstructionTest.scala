package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.TryInstruction.{Catch_, Try_, nextTryBranchId, toRetryIndex}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.{BranchId, Position}
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TryInstructionTest extends FreeSpec
{
  private val try_ = TryInstruction(
    tryWorkflow = Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/TRY")))),
    catchWorkflow = Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/CATCH")))))

  "JSON" in {
    testJson[Instruction.Labeled](try_,
      json"""{
        "TYPE": "Try",
        "try": {
          "instructions": [
            { "TYPE": "Execute.Anonymous", "job": { "agentRefPath": "/AGENT", "executablePath": "/TRY", "taskLimit": 1 }}
          ]
        },
        "catch": {
          "instructions": [
            { "TYPE": "Execute.Anonymous", "job": { "agentRefPath": "/AGENT", "executablePath": "/CATCH", "taskLimit": 1 }}
          ]
        }
      }""")
  }

  "workflow" in {
    assert(try_.workflow(Try_) == Valid(try_.tryWorkflow))
    assert(try_.workflow(Catch_) == Valid(try_.catchWorkflow))
    assert(try_.workflow("A").isInvalid)
  }

  "flattenedWorkflows" in {
    assert(try_.flattenedWorkflows(Position(7)) ==
      ((Position(7) / Try_) -> try_.tryWorkflow) ::
      ((Position(7) / Catch_) -> try_.catchWorkflow) :: Nil)
  }

  "flattenedInstructions" in {
    assert(try_.flattenedInstructions(Position(7)) == Vector[(Position, Instruction.Labeled)](
      Position(7) / Try_ % 0 -> try_.tryWorkflow.instructions(0),
      Position(7) / Try_ % 1 -> ImplicitEnd,
      Position(7) / Catch_ % 0 -> try_.catchWorkflow.instructions(0),
      Position(7) / Catch_ % 1 -> ImplicitEnd))
  }

  "normalizeBranchId" in {
    assert(try_.normalizeBranchId("X") == BranchId("X"))  // error, should be Try_ or Catch_. We don't care here
    assert(try_.normalizeBranchId(0) == BranchId(0))      // error, should be Try_ or Catch_. We don't care here
    assert(try_.normalizeBranchId("try") == Try_)
    assert(try_.normalizeBranchId("try-1") == Try_)
    assert(try_.normalizeBranchId("try-123") == Try_)
    assert(try_.normalizeBranchId("catch") == Catch_)
    assert(try_.normalizeBranchId("catch-123") == Catch_)
  }

  "toCatchBranchId" in {
    assert(try_.toCatchBranchId("X") == None)
    assert(try_.toCatchBranchId(Try_) == Some(Catch_))
    assert(try_.toCatchBranchId("try-1") == Some(BranchId("catch-1")))
    assert(try_.toCatchBranchId("try-123") == Some(BranchId("catch-123")))
    assert(try_.toCatchBranchId(Catch_) == None)
    assert(try_.toCatchBranchId("catch-1") == None)
  }

  "toRetryIndex" in {
    assert(toRetryIndex("X") == Invalid(Problem("Invalid BranchId for Try instruction: X")))
    assert(toRetryIndex(0) == Invalid(Problem("Invalid BranchId for Try instruction: 0")))
    assert(toRetryIndex("try") == Valid(0))
    assert(toRetryIndex("try-1") == Valid(1))
    assert(toRetryIndex("try-123") == Valid(123))
    assert(toRetryIndex("catch") == Valid(0))
    assert(toRetryIndex("catch-123") == Valid(123))
  }

  "nextTryBranchId" in {
    assert(nextTryBranchId("X") == Invalid(Problem("Invalid BranchId for nextTryBranchId: X")))
    assert(nextTryBranchId(0) == Invalid(Problem("Invalid BranchId for nextTryBranchId: 0")))
    assert(nextTryBranchId("try") == Valid(None))
    assert(nextTryBranchId("try-1") == Valid(None))
    assert(nextTryBranchId("try-2") == Valid(None))
    assert(nextTryBranchId("catch") == Valid(Some(BranchId("try-1"))))
    assert(nextTryBranchId("catch-1") == Valid(Some(BranchId("try-2"))))
    assert(nextTryBranchId("catch-123") == Valid(Some(BranchId("try-124"))))
  }
}

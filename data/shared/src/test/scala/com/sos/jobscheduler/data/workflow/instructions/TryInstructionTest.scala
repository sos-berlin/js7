package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.BranchId.{Catch_, Try_}
import com.sos.jobscheduler.data.workflow.position.{BranchId, Position}
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class TryInstructionTest extends FreeSpec
{
  private val try_ = TryInstruction(
    tryWorkflow = Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/TRY")))),
    catchWorkflow = Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/CATCH")))))

  "JSON" - {
    "with defaults" in {
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

    "complete" in {
      testJson[Instruction.Labeled](
        TryInstruction(
          tryWorkflow = Workflow.of(Fail(None)),
          catchWorkflow = Workflow.empty,
          retryDelays = 100.milliseconds :: 1.minute :: Nil,
          Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Try",
          "try": {
            "instructions": [
              { "TYPE": "Fail" }
            ]
          },
          "catch": {
            "instructions": []
          },
         "retryDelays": [ 0.1, 60 ],
         "sourcePos": [ 1, 2 ]
        }""")
    }
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
      Position(7) / Try_ % 1 -> ImplicitEnd(),
      Position(7) / Catch_ % 0 -> try_.catchWorkflow.instructions(0),
      Position(7) / Catch_ % 1 -> ImplicitEnd()))
  }

  "toCatchBranchId" in {
    assert(try_.toCatchBranchId("X") == None)
    assert(try_.toCatchBranchId(Try_) == Some(Catch_))
    assert(try_.toCatchBranchId("try+1") == Some(BranchId("catch+1")))
    assert(try_.toCatchBranchId("try+123") == Some(BranchId("catch+123")))
    assert(try_.toCatchBranchId(Catch_) == None)
    assert(try_.toCatchBranchId("catch+1") == None)
  }

  "retryCount" in {
    def t(delays: Seq[FiniteDuration]) = TryInstruction(Workflow.empty, Workflow.empty, delays)
    assert(t(Nil).retryDelay(1) == 0.seconds)
    assert(t(Nil).retryDelay(2) == 0.seconds)
    assert(t(Nil).retryDelay(3) == 0.seconds)
    assert(t(1.second :: Nil).retryDelay(1) == 1.seconds)
    assert(t(1.second :: Nil).retryDelay(2) == 1.seconds)
    assert(t(1.second :: 2.seconds :: Nil).retryDelay(1) == 1.seconds)
    assert(t(1.second :: 2.seconds :: Nil).retryDelay(2) == 2.seconds)
    assert(t(1.second :: 2.seconds :: Nil).retryDelay(3) == 2.seconds)
  }
}

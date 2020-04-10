package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.expression.Expression.BooleanConstant
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.BranchId.{Catch_, Try_}
import com.sos.jobscheduler.data.workflow.position.{BranchId, Position}
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
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
              {
                "TYPE": "Execute.Anonymous",
                "job": {
                  "agentRefPath": "/AGENT",
                  "executable": {
                    "TYPE": "ExecutablePath",
                    "path": "/TRY"
                  },
                  "taskLimit": 1
                }
              }
            ]
          },
          "catch": {
            "instructions": [
              {
                "TYPE": "Execute.Anonymous",
                "job": {
                  "agentRefPath": "/AGENT",
                  "executable": {
                    "TYPE": "ExecutablePath",
                    "path": "/CATCH"
                  },
                  "taskLimit": 1
                }
              }
            ]
          }
        }""")
    }

    "complete" in {
      testJson[Instruction.Labeled](
        TryInstruction(
          tryWorkflow = Workflow.of(Fail(None)),
          catchWorkflow = Workflow.of(Retry()),
          retryDelays = Some(Vector(100.milliseconds, 1.minute)),
          maxTries = Some(10),
          Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Try",
          "try": {
            "instructions": [
              { "TYPE": "Fail" }
            ]
          },
          "catch": {
            "instructions": [
              { "TYPE": "Retry" }
            ]
          },
         "retryDelays": [ 0.1, 60 ],
         "maxTries": 10,
         "sourcePos": [ 1, 2 ]
        }""")
    }
  }

  "retryDelays require a retry instruction" - {
    "no retry" in {
      assert(
        TryInstruction.checked(
          Workflow.empty,
          Workflow.empty,
          Some(1.second :: Nil))
        == Left(TryInstruction.MissingRetryProblem))
    }

    "retry nested in if-then-try is okay" in {
      val Right(try_) = TryInstruction.checked(
        Workflow.empty,
        Workflow.of(
          If(BooleanConstant(true),
            Workflow.of(
              If(BooleanConstant(true),
                Workflow.of(
                  TryInstruction(
                    Workflow.of(Retry()),  // This retry belongs to the outer catch-block
                    Workflow.empty)))))),
        Some(1.second :: Nil))
      assert(try_.isRetry)
    }

    "retry nested in if-else is okay" in {
      val Right(try_) = TryInstruction.checked(
        Workflow.empty,
        Workflow.of(
          If(BooleanConstant(true),
            Workflow.empty,
            Some(Workflow.of(
              If(BooleanConstant(true),
                Workflow.empty,
                Some(Workflow.of(Retry()))))))),
        Some(1.second :: Nil))
      assert(try_.isRetry)
    }

    "retry in try is not okay" in {
      val Right(try_) = TryInstruction.checked(
        Workflow.empty,
        Workflow.of(
          If(BooleanConstant(true),
            Workflow.empty,
            Some(Workflow.of(
              If(BooleanConstant(true),
                Workflow.empty,
                Some(Workflow.of(Retry()))))))),
        Some(1.second :: Nil))
      assert(try_.isRetry)
    }
  }

  "workflow" in {
    assert(try_.workflow(Try_) == Right(try_.tryWorkflow))
    assert(try_.workflow(Catch_) == Right(try_.catchWorkflow))
    assert(try_.workflow("A").isLeft)
  }

  "flattenedBranchToWorkflow" in {
    assert(try_.flattenedWorkflows(Position(7)) ==
      ((Position(7) / "try") -> try_.tryWorkflow) ::
      ((Position(7) / "catch") -> try_.catchWorkflow) :: Nil)
  }

  "flattenedInstructions" in {
    assert(try_.flattenedInstructions(Position(7)) == Vector[(Position, Instruction.Labeled)](
      Position(7) / "try" % 0 -> try_.tryWorkflow.instructions(0),
      Position(7) / "try" % 1 -> ImplicitEnd(),
      Position(7) / "catch" % 0 -> try_.catchWorkflow.instructions(0),
      Position(7) / "catch" % 1 -> ImplicitEnd()))
  }

  "toCatchBranchId" in {
    assert(try_.toCatchBranchId("X") == None)
    assert(try_.toCatchBranchId("try+0") == Some(BranchId("catch+0")))
    assert(try_.toCatchBranchId("try+1") == Some(BranchId("catch+1")))
    assert(try_.toCatchBranchId("try+123") == Some(BranchId("catch+123")))
    assert(try_.toCatchBranchId("catch+0") == None)
    assert(try_.toCatchBranchId("catch+1") == None)
  }

  "retryCount" in {
    def t(delays: Option[Seq[FiniteDuration]]) = TryInstruction(Workflow.empty, Workflow.empty, delays.map(_.toVector))
    assert(t(None).retryDelay(1) == 0.seconds)
    assert(t(None).retryDelay(2) == 0.seconds)
    assert(t(Some(Nil)).retryDelay(1) == 0.seconds)
    assert(t(Some(Nil)).retryDelay(2) == 0.seconds)
    assert(t(Some(Nil)).retryDelay(3) == 0.seconds)
    assert(t(Some(1.second :: Nil)).retryDelay(1) == 1.seconds)
    assert(t(Some(1.second :: Nil)).retryDelay(2) == 1.seconds)
    assert(t(Some(1.second :: 2.seconds :: Nil)).retryDelay(1) == 1.seconds)
    assert(t(Some(1.second :: 2.seconds :: Nil)).retryDelay(2) == 2.seconds)
    assert(t(Some(1.second :: 2.seconds :: Nil)).retryDelay(3) == 2.seconds)
  }
}

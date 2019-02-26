package com.sos.jobscheduler.core.workflow

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.core.workflow.InstructionContextTest._
import com.sos.jobscheduler.data.workflow.instructions.TryInstruction.{Catch_, Try_, catch_, try_}
import com.sos.jobscheduler.data.workflow.instructions.{Fail, Retry, TryInstruction}
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowPosition}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class InstructionContextTest extends FreeSpec
{
  private lazy val context = new InstructionContext {
    def instruction(workflowPosition: WorkflowPosition) = TestWorkflow.instruction(workflowPosition.position)
  }

  "toRetryCount" in {
    assert(context.toRetryCount(TestWorkflow.id /: Position(0)) == 0)    // No retry position
    assert(context.toRetryCount(TestWorkflow.id /: Position(99)) == 0)   // No instruction
    assert(context.toRetryCount(TestWorkflow.id /: (Position(1) / Try_ % 0)) == 0)
    assert(context.toRetryCount(TestWorkflow.id /: (Position(1) / try_(1) % 0)) == 1)
    assert(context.toRetryCount(TestWorkflow.id /: (Position(1) / try_(2) % 0)) == 2)
    assert(context.toRetryCount(TestWorkflow.id /: (Position(1) / Catch_ % 0)) == 0)
    assert(context.toRetryCount(TestWorkflow.id /: (Position(1) / catch_(1) % 0)) == 1)
    assert(context.toRetryCount(TestWorkflow.id /: (Position(1) / catch_(2) % 0)) == 2)
  }

  "toRetryPosition" in {
    assert(context.toRetryPosition(TestWorkflow.id /: Position(0)) == Invalid(Problem("Not in a catch-block")))
    assert(context.toRetryPosition(TestWorkflow.id /: Position(99)) == Invalid(Problem("Not in a catch-block")))
    assert(context.toRetryPosition(TestWorkflow.id /: (Position(1) / Try_ % 0)) == Invalid(Problem("Not in a catch-block")))
    assert(context.toRetryPosition(TestWorkflow.id /: (Position(1) / try_(1) % 0)) == Invalid(Problem("Not in a catch-block")))
    assert(context.toRetryPosition(TestWorkflow.id /: (Position(1) / Catch_ % 0)) == Valid(Position(1) / try_(1) % 0))
    assert(context.toRetryPosition(TestWorkflow.id /: (Position(1) / catch_(1) % 0)) == Valid(Position(1) / try_(2) % 0))
    assert(context.toRetryPosition(TestWorkflow.id /: (Position(1) / catch_(2) % 0)) == Valid(Position(1) / try_(3) % 0))
  }
}

object InstructionContextTest {
  private val TestWorkflow = Workflow(WorkflowPath("/WORKFLOW") % "VERSION", Vector(
    Fail,
    TryInstruction(
      Workflow.of(Fail),
      Workflow.of(TryInstruction(
        Workflow.of(Fail),
        Workflow.of(Retry()))))))
}

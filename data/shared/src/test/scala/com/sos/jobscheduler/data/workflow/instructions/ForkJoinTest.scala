package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ForkJoinTest extends FreeSpec {

  private val forkJoin = ForkJoin.of(
    "A" → Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/A")))),
    "B" → Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/B")))))

  "JSON" in {
    testJson[Instruction.Labeled](
      forkJoin,
      json"""{
        "TYPE": "ForkJoin",
        "branches": [
          {
            "id": "A",
            "workflow": {
              "instructions": [
                { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/A", "taskLimit": 1 }}
              ]
            }
          }, {
            "id": "B",
            "workflow": {
              "instructions": [
                { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/B", "taskLimit": 1 }}
              ]
            }
          }
        ]
      }""")
  }

  "workflow" in {
    assert(forkJoin.workflow("A") == Valid(forkJoin.branches(0).workflow))
    assert(forkJoin.workflow("B") == Valid(forkJoin.branches(1).workflow))
    assert(forkJoin.workflow(1).isInvalid)
  }

  "flattenedWorkflows" in {
    assert(forkJoin.flattenedWorkflows(Position(7)) ==
      ((Position(7) / "A") → forkJoin.branches(0).workflow) ::
      ((Position(7) / "B") → forkJoin.branches(1).workflow) :: Nil)
  }

  "flattenedInstructions" in {
    assert(forkJoin.flattenedInstructions(Position(7)) == Vector[(Position, Instruction.Labeled)](
      Position(7, "A", 0) → forkJoin.branches(0).workflow.instructions(0),
      Position(7, "A", 1) → ImplicitEnd,
      Position(7, "B", 0) → forkJoin.branches(1).workflow.instructions(0),
      Position(7, "B", 1) → ImplicitEnd))
  }
}

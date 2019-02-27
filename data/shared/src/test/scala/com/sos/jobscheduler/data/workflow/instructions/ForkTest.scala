package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.ProblemException
import com.sos.jobscheduler.data.agent.AgentRefPath
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
final class ForkTest extends FreeSpec {

  private val fork = Fork.of(
    "A" -> Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A")))),
    "B" -> Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/B")))))

  "JSON" in {
    testJson[Instruction.Labeled](
      fork,
      json"""{
        "TYPE": "Fork",
        "branches": [
          {
            "id": "A",
            "workflow": {
              "instructions": [
                { "TYPE": "Execute.Anonymous", "job": { "agentRefPath": "/AGENT", "executablePath": "/A", "taskLimit": 1 }}
              ]
            }
          }, {
            "id": "B",
            "workflow": {
              "instructions": [
                { "TYPE": "Execute.Anonymous", "job": { "agentRefPath": "/AGENT", "executablePath": "/B", "taskLimit": 1 }}
              ]
            }
          }
        ]
      }""")
  }

  "Duplicate branch ids are rejected" in {  // TODO
    intercept[ProblemException] {
      Fork.of(
        "A" -> Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A")))),
        "A" -> Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/B")))))
    }
  }

  "workflow" in {
    assert(fork.workflow("fork+A") == Valid(fork.branches(0).workflow))
    assert(fork.workflow("fork+B") == Valid(fork.branches(1).workflow))
    assert(fork.workflow(1).isInvalid)
  }

  "flattenedWorkflows" in {
    assert(fork.flattenedWorkflows(Position(7)) ==
      ((Position(7) / "fork+A") -> fork.branches(0).workflow) ::
      ((Position(7) / "fork+B") -> fork.branches(1).workflow) :: Nil)
  }

  "flattenedInstructions" in {
    assert(fork.flattenedInstructions(Position(7)) == Vector[(Position, Instruction.Labeled)](
      Position(7) / "fork+A" % 0 -> fork.branches(0).workflow.instructions(0),
      Position(7) / "fork+A" % 1 -> ImplicitEnd,
      Position(7) / "fork+B" % 0 -> fork.branches(1).workflow.instructions(0),
      Position(7) / "fork+B" % 1 -> ImplicitEnd))
  }
}

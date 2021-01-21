package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.base.problem.ProblemException
import js7.data.agent.AgentId
import js7.data.job.PathExecutable
import js7.data.source.SourcePos
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ForkTest extends AnyFreeSpec {

  private val fork = Fork.of(
    "A" -> Workflow.of(Execute(WorkflowJob(AgentId("AGENT"), PathExecutable("A")))),
    "B" -> Workflow.of(Execute(WorkflowJob(AgentId("AGENT"), PathExecutable("B")))))
    .copy(sourcePos = Some(SourcePos(1, 2)))

  "JSON" - {
    testJson[Instruction.Labeled](
      fork,
      json"""{
        "TYPE": "Fork",
        "branches": [
          {
            "id": "A",
            "workflow": {
              "instructions": [
                {
                  "TYPE": "Execute.Anonymous",
                  "job": {
                    "agentId": "AGENT",
                    "executable": {
                      "TYPE": "PathExecutable",
                      "path": "A"
                    },
                    "taskLimit": 1
                  }
                }
              ]
            }
          }, {
            "id": "B",
            "workflow": {
              "instructions": [
                {
                  "TYPE": "Execute.Anonymous",
                  "job": {
                    "agentId": "AGENT",
                    "executable": {
                      "TYPE": "PathExecutable",
                      "path": "B"
                    },
                    "taskLimit": 1
                  }
                }
              ]
            }
          }
        ],
        "sourcePos": [ 1, 2 ]
      }""")
  }

  "Duplicate branch ids are rejected" in {  // TODO
    intercept[ProblemException] {
      Fork.of(
        "A" -> Workflow.of(Execute(WorkflowJob(AgentId("AGENT"), PathExecutable("A")))),
        "A" -> Workflow.of(Execute(WorkflowJob(AgentId("AGENT"), PathExecutable("B")))))
    }
  }

  "workflow" in {
    assert(fork.workflow("fork+A") == Right(fork.branches(0).workflow))
    assert(fork.workflow("fork+B") == Right(fork.branches(1).workflow))
    assert(fork.workflow("X").isLeft)
  }

  "flattenedWorkflows" in {
    assert(fork.flattenedWorkflows(Position(7)) ==
      ((Position(7) / "fork+A") -> fork.branches(0).workflow) ::
      ((Position(7) / "fork+B") -> fork.branches(1).workflow) :: Nil)
  }

  "flattenedInstructions" in {
    assert(fork.flattenedInstructions(Position(7)) == Vector[(Position, Instruction.Labeled)](
      Position(7) / "fork+A" % 0 -> fork.branches(0).workflow.instructions(0),
      Position(7) / "fork+A" % 1 -> ImplicitEnd(),
      Position(7) / "fork+B" % 0 -> fork.branches(1).workflow.instructions(0),
      Position(7) / "fork+B" % 1 -> ImplicitEnd()))
  }
}

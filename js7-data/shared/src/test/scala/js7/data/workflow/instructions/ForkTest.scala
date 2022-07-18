package js7.data.workflow.instructions

import cats.syntax.semigroup.*
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.data.agent.AgentPath
import js7.data.job.PathExecutable
import js7.data.source.SourcePos
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.FastparseExpressionParser.expr
import js7.data.workflow.instructions.Fork.DuplicatedBranchIdsInForkProblem
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ForkTest extends AnyFreeSpec
{
  private val fork =
    Fork(
      Vector(
        Fork.Branch(
          "A",
          Workflow.anonymous(
            Vector(Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("A")))),
            result = Some(Map("aResult" -> expr("$RESULT"))))),
        Fork.Branch(
          "B",
          Workflow.of(Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("B")))))),
      agentPath = Some(AgentPath("AGENT")),
      joinIfFailed = true,
      sourcePos = Some(SourcePos(1, 2)))

  "JSON" - {
    "complete" in {
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
                      "agentPath": "AGENT",
                      "executable": {
                        "TYPE": "PathExecutable",
                        "path": "A"
                      },
                      "parallelism": 1
                    }
                  }
                ],
                "result": {
                  "aResult": "$$RESULT"
                }
              }
            }, {
              "id": "B",
              "workflow": {
                "instructions": [
                  {
                    "TYPE": "Execute.Anonymous",
                    "job": {
                      "agentPath": "AGENT",
                      "executable": {
                        "TYPE": "PathExecutable",
                        "path": "B"
                      },
                      "parallelism": 1
                    }
                  }
                ]
              }
            }
          ],
          "agentPath": "AGENT",
          "joinIfFailed": true,
          "sourcePos": [ 1, 2 ]
        }""")
    }

    "minimum" in {
      testJson[Instruction.Labeled](
        Fork(Vector(
          Fork.Branch("A", Workflow.empty))),
        json"""{
          "TYPE": "Fork",
          "branches": [
            {
              "id": "A",
              "workflow": {
                "instructions": []
              }
            }
          ]
        }""")
    }
  }

  "Duplicate branch ids are rejected" in {  // TODO
    val checked = Fork.checked(Seq(
      Fork.Branch("A", Workflow.empty),
      Fork.Branch("A", Workflow.empty)))
    assert(checked == Left(DuplicatedBranchIdsInForkProblem(Seq(Fork.Branch.Id("A")))))
  }

  "Duplicate result names are rejected" in {
    val expr = StringConstant.empty
    val checked = Fork.checked(Seq(
      Fork.Branch("A", Workflow.anonymous(Nil, result = Some(Map("X" -> expr, "Z" -> expr)))),
      Fork.Branch("B", Workflow.anonymous(Nil, result = Some(Map("X" -> expr, "Y" -> expr)))),
      Fork.Branch("C", Workflow.anonymous(Nil, result = Some(Map("Y" -> expr, "Å" -> expr))))))
    assert(checked == Left(
      Problem("Result name 'X' is used duplicately in Fork branches 'A' , 'B'") |+|
      Problem("Result name 'Y' is used duplicately in Fork branches 'B' , 'C'")))
  }

  "workflow" in {
    assert(fork.workflow("fork+A") == Right(fork.branches(0).workflow))
    assert(fork.workflow("fork+B") == Right(fork.branches(1).workflow))
    assert(fork.workflow("X").isLeft)
  }

  "flattenedWorkflows" in {
    assert(fork.flattenedWorkflows(Position(7)).toSeq == Seq(
      (Position(7) / "fork+A") -> fork.branches(0).workflow,
      (Position(7) / "fork+B") -> fork.branches(1).workflow))
  }

  "flattenedInstructions" in {
    assert(fork.flattenedInstructions(Position(7)).toSeq == Seq[(Position, Instruction.Labeled)](
      Position(7) / "fork+A" % 0 -> fork.branches(0).workflow.instructions(0),
      Position(7) / "fork+A" % 1 -> ImplicitEnd(),
      Position(7) / "fork+B" % 0 -> fork.branches(1).workflow.instructions(0),
      Position(7) / "fork+B" % 1 -> ImplicitEnd()))
  }
}

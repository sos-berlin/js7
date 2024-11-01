package js7.data.workflow.instructions

import cats.data.NonEmptyVector
import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.utils.CatsUtils.Nev
import js7.data.agent.AgentPath
import js7.data.job.PathExecutable
import js7.data.source.SourcePos
import js7.data.value.expression.Expression.*
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.BranchId.{Else, Then}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

/**
  * @author Joacim Zschimmer
  */
final class IfTest extends OurTestSuite:

  private val if_ = If(
    Nev.one(If.IfThen(
      GreaterOrEqual(LastReturnCode, NumericConstant(3)),
      thenBlock = Workflow.of(Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("THEN")))))),
    elseBlock = Some(Workflow.of(Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("ELSE"))))),
    Some(SourcePos(1, 2)))

  "JSON" in:
    testJson[Instruction.Labeled](
      If(expr("true")):
        Workflow.empty,
      json"""{
        "TYPE": "If",
        "ifThens": [{
          "predicate": "true",
          "then": {
            "instructions": []
          }
        }]
      }""")

    testJson[Instruction.Labeled](
      If(
        Nev.of(
          If.IfThen(expr("false"), Workflow.empty),
          If.IfThen(expr("true"), Workflow.empty)),
        Some(Workflow.of(Stop()))),
      json"""{
        "TYPE": "If",
        "ifThens": [
          {
            "predicate": "false",
            "then": {
              "instructions": []
            }
          }, {
            "predicate": "true",
            "then": {
              "instructions": []
            }
          }
        ],
        "else": {
          "instructions": [
            {
              "TYPE": "Stop"
            }
          ]
        }
      }""")

    testJson[Instruction.Labeled](if_,
      json"""{
        "TYPE": "If",
        "ifThens": [{
          "predicate": "$$returnCode >= 3",
          "then": {
            "instructions": [
              {
                "TYPE": "Execute.Anonymous",
                "job": {
                  "agentPath": "AGENT",
                  "executable": {
                    "TYPE": "PathExecutable",
                    "path": "THEN"
                  },
                  "processLimit": 1
                }
              }
            ]
          }
        }],
        "else": {
          "instructions": [
            {
              "TYPE": "Execute.Anonymous",
              "job":  {
                "agentPath": "AGENT",
                "executable": {
                  "TYPE": "PathExecutable",
                  "path": "ELSE"
                },
                "processLimit": 1
              }
            }
          ]
        },
        "sourcePos": [ 1, 2 ]
      }""")

  "JSON until v2.7.1" in :
    testJsonDecoder[Instruction.Labeled](
      If(expr("true")):
        Workflow.empty,
      json"""{
        "TYPE": "If",
        "predicate": "true",
        "then": {
          "instructions": []
        }
      }""")

  "workflow" in:
    assert(if_.workflow(Then) == Right(if_.ifThens.head.thenBlock))
    assert(if_.workflow(Else) == Right(if_.elseBlock.get))
    assert(if_.workflow("A").isLeft)

  "flattenedBranchToWorkflow" in:
    assert(if_.flattenedWorkflows(Position(7)).toSeq == Seq(
      (Position(7) / Then) -> if_.ifThens.head.thenBlock,
      (Position(7) / Else) -> if_.elseBlock.get))

  "flattenedInstructions" in:
    assert(if_.flattenedInstructions(Position(7)).toSeq == Seq[(Position, Instruction.Labeled)](
      Position(7) / Then % 0 -> if_.ifThens.head.thenBlock.instructions(0),
      Position(7) / Then % 1 -> ImplicitEnd(),
      Position(7) / Else % 0 -> if_.elseBlock.get.instructions(0),
      Position(7) / Else % 1 -> ImplicitEnd()))

  "if-then-else Scala builder" in :
    val instr =
      If(expr("false")).Then:
        Workflow.empty
      .elseIf(expr("false")).Then:
        Workflow.empty
      .elseIf(expr("true")).Then:
        Workflow.empty
      .Else:
        Workflow.empty

    assert(instr == If(
      NonEmptyVector.of(
        If.IfThen(expr("false"), Workflow.empty),
        If.IfThen(expr("false"), Workflow.empty),
        If.IfThen(expr("true"), Workflow.empty)),
      Some(Workflow.empty)))

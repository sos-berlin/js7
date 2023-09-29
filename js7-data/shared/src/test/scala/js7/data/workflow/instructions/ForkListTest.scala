package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.job.PathExecutable
import js7.data.source.SourcePos
import js7.data.value.expression.ExpressionParser.{expr, exprFunction}
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class ForkListTest extends OurTestSuite
{
  private val fork = ForkList(
    expr("$children"),
    exprFunction("(listElement) => $listElement.number"),
    exprFunction("""(listElement) => {
        myId: $listElement.number,
        myString: $child.string
      }"""),
    Workflow.of(Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("A"))))
  ).copy(sourcePos = Some(SourcePos(1, 2)))

  "JSON" in {
    testJson[Instruction.Labeled](
      fork,
      json"""{
        "TYPE": "ForkList",
        "children": "$$children",
        "childToId": "(listElement)=>$$listElement.number",
        "childToArguments": "(listElement)=>{myId:$$listElement.number, myString:$$child.string}",
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
          ]
        },
        "joinIfFailed": false,
        "sourcePos": [ 1, 2 ]
      }""")

    testJson[Instruction.Labeled](
      ForkList(
        expr("$children"),
        exprFunction("(listElement) => $listElement"),
        exprFunction("(listElement) => { myId: $listElement }"),
        Workflow.empty,
        Some(AgentPath("AGENT"))),
      json"""{
        "TYPE": "ForkList",
        "children": "$$children",
        "childToId": "(listElement)=>$$listElement",
        "childToArguments": "(listElement)=>{myId:$$listElement}",
        "workflow": {
          "instructions": []
        },
        "agentPath": "AGENT",
        "joinIfFailed": false
      }""")

    testJsonDecoder[Instruction.Labeled](
      ForkList(
        expr("$children"),
        exprFunction("(listElement) => $listElement"),
        exprFunction("(listElement) => { myId: $listElement }"),
        Workflow.empty),
      json"""{
        "TYPE": "ForkList",
        "children": "$$children",
        "childToId": "(listElement)=>$$listElement",
        "childToArguments": "(listElement)=>{myId:$$listElement}",
        "workflow": {
          "instructions": []
        }
      }""")

    assert(
      json"""{
        "TYPE": "ForkList",
        "children": "$$children",
        "childToId": "() => 1",
        "childToArguments": "(listElement) => { myId: $$listElement }",
        "workflow": {
          "instructions": []
        },
        "agentPath": "AGENT"
      }""".as[ForkList].toChecked == Left(Problem(
        "JSON DecodingFailure at : The 'childToId' function is expected to accept between 1 and 2 parameters")))

    assert(
      json"""{
        "TYPE": "ForkList",
        "children": "$$children",
        "childToId": "(listElement) => $$listElement",
        "childToArguments": "(element, i, x) => {}",
        "workflow": {
          "instructions": []
        },
        "agentPath": "AGENT"
      }""".as[ForkList].toChecked == Left(Problem(
        "JSON DecodingFailure at : The 'childToArguments' function is expected to accept between 0 and 2 parameters")))
  }

  "workflow" in {
    assert(fork.workflow("fork") == Right(fork.workflow))
    assert(fork.workflow("X").isLeft)
  }

  "flattenedWorkflows" in {
    assert(fork.flattenedWorkflows(Position(7)).toSeq == Seq(
      (Position(7) / "fork") -> fork.workflow))
  }

  "flattenedInstructions" in {
    assert(fork.flattenedInstructions(Position(7)).toSeq == Seq[(Position, Instruction.Labeled)](
      Position(7) / "fork" % 0 -> fork.workflow.instructions(0),
      Position(7) / "fork" % 1 -> ImplicitEnd()))
  }
}

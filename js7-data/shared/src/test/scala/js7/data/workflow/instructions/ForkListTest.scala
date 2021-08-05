package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.data.agent.AgentPath
import js7.data.job.PathExecutable
import js7.data.source.SourcePos
import js7.data.value.expression.ExpressionParser.{expr, exprFunction}
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class ForkListTest extends AnyFreeSpec
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
        "sourcePos": [ 1, 2 ]
      }""")
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

package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.data.agent.AgentName
import js7.data.job.{ExecutablePath, ExecutableScript}
import js7.data.source.SourcePos
import js7.data.value.StringValue
import js7.data.value.expression.Expression.{NamedValue, NumericConstant, ObjectExpression}
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExecuteTest extends AnyFreeSpec
{
  "JSON" - {
    "Named with defaults" in {
      testJson[Instruction.Labeled](
        Execute.Named(WorkflowJob.Name("JOB")),
        json"""{
          "TYPE": "Execute.Named",
          "jobName": "JOB"
        }""")
    }

    "Named complete" in {
      testJson[Instruction.Labeled](
        Execute.Named(WorkflowJob.Name("JOB"), Map("ARG" -> StringValue("VALUE")), Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Execute.Named",
          "jobName": "JOB",
          "defaultArguments": {
            "ARG": "VALUE"
          },
          "sourcePos": [ 1, 2 ]
        }""")
    }

    "Anonymous with defaults" in {
      testJson[Instruction.Labeled](
        Execute(
          WorkflowJob(
            AgentName("AGENT"),
            ExecutablePath("EXECUTABLE"))),
        json"""{
          "TYPE": "Execute.Anonymous",
          "job": {
            "agentName": "AGENT",
            "executable": {
              "TYPE": "ExecutablePath",
              "path": "EXECUTABLE"
            },
            "taskLimit": 1
          }
        }""")
    }

    "Anonymous complete" in {
      testJson[Instruction.Labeled](
        Execute.Anonymous(
          WorkflowJob(
            AgentName("AGENT"),
            ExecutableScript("SCRIPT",
              ObjectExpression(Map(
                "ENV-VAR" -> NamedValue.last("VAR"),
                "NUMBER" -> NumericConstant(7)))),
            Map("ARG" -> StringValue("VALUE"))),
          Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Execute.Anonymous",
          "job": {
            "agentName": "AGENT",
            "executable": {
              "TYPE": "ExecutableScript",
              "script": "SCRIPT",
              "env": {
                "ENV-VAR": "$$VAR",
                "NUMBER": "7"
              }
            },
            "taskLimit": 1,
            "defaultArguments": {
              "ARG": "VALUE"
            }
          },
          "sourcePos": [ 1, 2 ]
        }""")
    }
  }
}

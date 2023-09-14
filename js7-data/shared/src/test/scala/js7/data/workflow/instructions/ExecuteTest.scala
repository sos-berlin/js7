package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.job.{PathExecutable, ShellScriptExecutable}
import js7.data.source.SourcePos
import js7.data.value.expression.Expression.{NamedValue, NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class ExecuteTest extends OurTestSuite
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
        Execute.Named(WorkflowJob.Name("JOB"), Map("ARG" -> StringConstant("VALUE")), Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Execute.Named",
          "jobName": "JOB",
          "defaultArguments": {
            "ARG": "'VALUE'"
          },
          "sourcePos": [ 1, 2 ]
        }""")
    }

    "Anonymous with defaults" in {
      testJson[Instruction.Labeled](
        Execute.Anonymous(
          WorkflowJob(
            AgentPath("AGENT"),
            PathExecutable("EXECUTABLE"))),
        json"""{
          "TYPE": "Execute.Anonymous",
          "job": {
            "agentPath": "AGENT",
            "executable": {
              "TYPE": "PathExecutable",
              "path": "EXECUTABLE"
            },
            "parallelism": 1
          }
        }""")
    }

    "Anonymous complete" in {
      testJson[Instruction.Labeled](
        Execute.Anonymous(
          WorkflowJob(
            AgentPath("AGENT"),
            ShellScriptExecutable("SCRIPT",
              Map(
                "ENV-VAR" -> NamedValue("VAR"),
                "NUMBER" -> NumericConstant(7))),
            Map("ARG" -> StringConstant("VALUE"))),
          Map("myArgument" -> expr("44")),
          Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Execute.Anonymous",
          "defaultArguments": {
            "myArgument": "44"
          },
          "job": {
            "agentPath": "AGENT",
            "executable": {
              "TYPE": "ShellScriptExecutable",
              "script": "SCRIPT",
              "env": {
                "ENV-VAR": "$$VAR",
                "NUMBER": "7"
              }
            },
            "parallelism": 1,
            "defaultArguments": {
              "ARG": "'VALUE'"
            }
          },
          "sourcePos": [ 1, 2 ]
        }""")
    }
  }
}

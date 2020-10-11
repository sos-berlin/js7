package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.data.agent.AgentName
import js7.data.job.{ExecutablePath, ExecutableScript}
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.tester.CirceJsonTester
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExecuteTest extends AnyFreeSpec
{
  "JSON" - {
    "Named with defaults" in {
      CirceJsonTester.testJson[Instruction.Labeled](
        Execute.Named(WorkflowJob.Name("JOB")),
        json"""{
          "TYPE": "Execute.Named",
          "jobName": "JOB"
        }""")
    }

    "Named complete" in {
      CirceJsonTester.testJson[Instruction.Labeled](
        Execute.Named(WorkflowJob.Name("JOB"), Map("ARG" -> "VALUE"), Some(SourcePos(1, 2))),
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
      CirceJsonTester.testJson[Instruction.Labeled](
        Execute(
          WorkflowJob(
            AgentName("AGENT"),
            ExecutablePath("/EXECUTABLE"))),
        json"""{
          "TYPE": "Execute.Anonymous",
          "job": {
            "agentName": "AGENT",
            "executable": {
              "TYPE": "ExecutablePath",
              "path": "/EXECUTABLE"
            },
            "taskLimit": 1
          }
        }""")
    }

    "Anonymous complete" in {
      CirceJsonTester.testJson[Instruction.Labeled](
        Execute.Anonymous(
          WorkflowJob(
            AgentName("AGENT"),
            ExecutableScript("SCRIPT"),
            Map("ARG" -> "VALUE")),
          Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Execute.Anonymous",
          "job": {
            "agentName": "AGENT",
            "executable": {
              "TYPE": "ExecutableScript",
              "script": "SCRIPT"
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

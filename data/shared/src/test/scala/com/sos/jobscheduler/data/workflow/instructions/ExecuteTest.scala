package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.{ExecutablePath, ExecutableScript}
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.tester.CirceJsonTester
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExecuteTest extends FreeSpec
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
            AgentRefPath("/AGENT"),
            ExecutablePath("/EXECUTABLE"))),
        json"""{
          "TYPE": "Execute.Anonymous",
          "job": {
            "agentRefPath": "/AGENT",
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
            AgentRefPath("/AGENT"),
            ExecutableScript("SCRIPT"),
            Map("ARG" -> "VALUE")),
          Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Execute.Anonymous",
          "job": {
            "agentRefPath": "/AGENT",
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

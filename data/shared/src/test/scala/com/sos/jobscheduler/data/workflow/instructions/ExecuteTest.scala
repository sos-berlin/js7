package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ExecutablePath
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
    "Named" in {
      CirceJsonTester.testJson[Instruction.Labeled](
        Execute.Named(WorkflowJob.Name("EXECUTABLE")),
        json"""{
          "TYPE": "Execute.Named",
          "name": "EXECUTABLE"
        }""")
    }

    "Named with defaultArguments" in {
      CirceJsonTester.testJson[Instruction.Labeled](
        Execute.Named(WorkflowJob.Name("EXECUTABLE"), Map("ARG" → "VALUE")),
        json"""{
          "TYPE": "Execute.Named",
          "name": "EXECUTABLE",
          "defaultArguments": {
            "ARG": "VALUE"
          }
        }""")
    }

    "Anonymous" in {
      CirceJsonTester.testJson[Instruction.Labeled](
        Execute(
          WorkflowJob(
            AgentPath("/AGENT"),
            ExecutablePath("/EXECUTABLE"))),
        json"""{
          "TYPE": "Execute.Anonymous",
          "job": {
            "agentPath": "/AGENT",
            "executablePath": "/EXECUTABLE",
            "taskLimit": 1
          }
        }""")
    }

    "Anonymous with defaultArguments" in {
      CirceJsonTester.testJson[Instruction.Labeled](
        Execute(
          WorkflowJob(
            AgentPath("/AGENT"),
            ExecutablePath("/EXECUTABLE"),
            Map("ARG" → "VALUE"))),
        json"""{
          "TYPE": "Execute.Anonymous",
          "job": {
            "agentPath": "/AGENT",
            "executablePath": "/EXECUTABLE",
            "taskLimit": 1,
            "defaultArguments": {
              "ARG": "VALUE"
            }
          }
        }""")
    }
  }
}

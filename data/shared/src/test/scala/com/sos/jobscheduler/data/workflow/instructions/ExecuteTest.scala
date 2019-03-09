package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
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
        Execute.Named(WorkflowJob.Name("EXECUTABLE")),
        json"""{
          "TYPE": "Execute.Named",
          "name": "EXECUTABLE"
        }""")
    }

    "Named complete" in {
      CirceJsonTester.testJson[Instruction.Labeled](
        Execute.Named(WorkflowJob.Name("EXECUTABLE"), Map("ARG" -> "VALUE"), Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Execute.Named",
          "name": "EXECUTABLE",
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
            "executablePath": "/EXECUTABLE",
            "taskLimit": 1
          }
        }""")
    }

    "Anonymous complete" in {
      CirceJsonTester.testJson[Instruction.Labeled](
        Execute.Anonymous(
          WorkflowJob(
            AgentRefPath("/AGENT"),
            ExecutablePath("/EXECUTABLE"),
            Map("ARG" -> "VALUE")),
          Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Execute.Anonymous",
          "job": {
            "agentRefPath": "/AGENT",
            "executablePath": "/EXECUTABLE",
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

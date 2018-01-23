package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Instruction._
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.{AgentJobPath, Instruction, JobPath, Label, Workflow}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.Json
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class InstructionsTest extends FreeSpec {

  "JSON" - {
    "With Label" in {
      testJson("A" @: ExplicitEnd: Labeled, json"""
        {
          "TYPE": "End",
          "labels": [ "A" ]
        }""")
    }

    "Without Label" in {
      testJson(() @: ExplicitEnd: Labeled, json"""
        {
          "TYPE": "End"
        }""")
    }

    "Job" in {
      testLabeled(Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB"))), json"""
        {
          "TYPE": "Job",
          "job": {
            "agentPath": "/AGENT",
            "jobPath": "/JOB"
          }
        }""")
    }

    "ForkJoin" in {
      testLabeled(ForkJoin.of(
        "A" → Workflow.of(Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB")))),
        "B" → Workflow()), json"""
        {
          "TYPE": "ForkJoin",
          "branches":
            [
              {
                "id": "A",
                "workflow": {
                  "instructions": [
                    { "TYPE": "Job", "job": { "agentPath": "/AGENT", "jobPath": "/JOB" } },
                    { "TYPE": "ImplicitEnd" }
                  ]
                }
              }, {
                "id": "B",
                "workflow": {
                  "instructions": [
                    { "TYPE": "ImplicitEnd" }
                  ]
                }
              }
            ]
        }""")
    }

    "Gap" in {
      testLabeled(Gap, json"""
        {
          "TYPE": "Gap"
        }""")
    }

    "ExplicitEnd" in {
      testLabeled(ExplicitEnd, json"""
        {
          "TYPE": "End"
        }""")
    }

    "Goto" in {
      testLabeled(Goto(Label("A")), json"""
        {
          "TYPE": "Goto",
          "to": "A"
        }""")
    }

    "IfErrorGoto" in {
      testLabeled(IfErrorGoto(Label("A")), json"""
        {
          "TYPE": "IfErrorGoto",
          "to": "A"
        }""")
    }

    def testLabeled(instruction: Instruction, json: Json) =
      testJson(Labeled(Nil, instruction), json)
  }
}

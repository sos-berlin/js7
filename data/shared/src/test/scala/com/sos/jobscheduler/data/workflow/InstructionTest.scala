package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Instruction._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.Json
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class InstructionTest extends FreeSpec {

  "@:" in {
    assert("LABEL" @: ExplicitEnd == Instruction.Labeled(Label("LABEL") :: Nil, ExplicitEnd))
    assert(() @: ExplicitEnd == Instruction.Labeled(Nil, ExplicitEnd))
    "LABEL" @: ExplicitEnd match {
      case (Label("LABEL") :: Nil) @: ExplicitEnd ⇒ // ok
      case _ ⇒ fail()
    }
  }

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
      testLabeled(ForkJoin(List(
        OrderId.ChildId("A") → Workflow(Vector(Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB"))))),
        OrderId.ChildId("B") → Workflow())), json"""
        {
          "TYPE": "ForkJoin",
          "idToWorkflow":
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

    "IfError" in {
      testLabeled(IfError(Label("A")), json"""
        {
          "TYPE": "IfError",
          "to": "A"
        }""")
    }

    def testLabeled(instruction: Instruction, json: Json) =
      testJson(Labeled(Nil, instruction), json)
  }
}

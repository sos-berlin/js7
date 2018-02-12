package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.workflow.{JobPath, Workflow}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IfReturnCodeTest extends FreeSpec
{
  "JSON" in {
    testJson(
      IfReturnCode(
        List(ReturnCode(1), ReturnCode(3)),
        thenWorkflow = Workflow.of(Job(JobPath("/THEN"), AgentPath("/AGENT"))),
        elseWorkflow = Some(Workflow.of(Job(JobPath("/ELSE"), AgentPath("/AGENT"))))),
      json"""{
        "returnCodes": [ 1, 3 ],
        "then": {
          "instructions": [
            { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/THEN" },
            { "TYPE": "ImplicitEnd" }
          ]
        },
        "else": {
          "instructions": [
            { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/ELSE" },
            { "TYPE": "ImplicitEnd" }
          ]
        }
      }""")
  }
}

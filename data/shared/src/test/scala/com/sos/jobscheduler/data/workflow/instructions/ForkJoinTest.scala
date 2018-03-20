package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec


/**
  * @author Joacim Zschimmer
  */
final class ForkJoinTest extends FreeSpec {

  "JSON" in {
    testJson[Instruction.Labeled](
      ForkJoin.of(
        "A" → Workflow.of(Job(JobPath("/A"), AgentPath("/AGENT"))),
        "B" → Workflow.of(Job(JobPath("/B"), AgentPath("/AGENT")))),
      json"""{
        "TYPE": "ForkJoin",
        "branches": [
          {
            "id": "A",
            "workflow": {
              "instructions": [
                { "TYPE": "Job", "jobPath": "/A", "agentPath": "/AGENT" }
              ]
            }
          }, {
            "id": "B",
            "workflow": {
              "instructions": [
                { "TYPE": "Job", "jobPath": "/B", "agentPath": "/AGENT" }
              ]
            }
          }
        ]
      }""")
  }
}

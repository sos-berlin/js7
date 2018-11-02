package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
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
        "A" → Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/A")))),
        "B" → Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/B"))))),
      json"""{
        "TYPE": "ForkJoin",
        "branches": [
          {
            "id": "A",
            "workflow": {
              "instructions": [
                { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/A", "taskLimit": 1 }}
              ]
            }
          }, {
            "id": "B",
            "workflow": {
              "instructions": [
                { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/B", "taskLimit": 1 }}
              ]
            }
          }
        ]
      }""")
  }
}

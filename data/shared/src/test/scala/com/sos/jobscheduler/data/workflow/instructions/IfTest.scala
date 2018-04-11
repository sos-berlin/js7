package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression._
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IfTest extends FreeSpec
{
  "JSON" in {
    testJson[Instruction.Labeled](
      If(
        GreaterOrEqual(OrderReturnCode, NumericConstant(3)),
        thenWorkflow = Workflow.of(Job(JobPath("/THEN"), AgentPath("/AGENT"))),
        elseWorkflow = Some(Workflow.of(Job(JobPath("/ELSE"), AgentPath("/AGENT"))))),
      json"""{
        "TYPE": "If",
        "predicate": "returnCode >= 3",
        "then": {
          "instructions": [
            { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/THEN" }
          ]
        },
        "else": {
          "instructions": [
            { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/ELSE" }
          ]
        }
      }""")
  }
}

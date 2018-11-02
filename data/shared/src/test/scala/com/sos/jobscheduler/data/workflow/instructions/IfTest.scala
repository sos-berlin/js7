package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
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
        thenWorkflow = Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/THEN")))),
        elseWorkflow = Some(Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/ELSE")))))),
      json"""{
        "TYPE": "If",
        "predicate": "returnCode >= 3",
        "then": {
          "instructions": [
            { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/THEN", "taskLimit": 1 }}
          ]
        },
        "else": {
          "instructions": [
            { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/ELSE", "taskLimit": 1 }}
          ]
        }
      }""")
  }
}

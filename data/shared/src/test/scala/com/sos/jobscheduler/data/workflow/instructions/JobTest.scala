package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.{JobPath, ReturnCode}
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JobTest extends FreeSpec {

  "JSON" - {
    "simple" in {
      testJson[Instruction.Labeled](
        Job(JobPath("/JOB"), AgentPath("/AGENT")),
        json"""{
          "TYPE": "Job",
          "jobPath": "/JOB",
          "agentPath": "/AGENT"
        }""")
    }

    "returnCodeMeaning success" in {
      testJson[Instruction.Labeled](
        Job(JobPath("/JOB"), AgentPath("/AGENT"), ReturnCodeMeaning.Success(Set(ReturnCode(0), ReturnCode(1)))),
        json"""{
          "TYPE": "Job",
          "jobPath": "/JOB",
          "agentPath": "/AGENT",
          "returnCodeMeaning": {
            "success": [ 0, 1 ]
          }
        }""")
    }

    "returnCodeMeaning failure" in {
      testJson[Instruction.Labeled](
        Job(JobPath("/JOB"), AgentPath("/AGENT"), ReturnCodeMeaning.Failure(Set(ReturnCode(99)))),
        json"""{
          "TYPE": "Job",
          "jobPath": "/JOB",
          "agentPath": "/AGENT",
          "returnCodeMeaning": {
            "failure": [ 99 ]
          }
        }""")
    }
  }
}

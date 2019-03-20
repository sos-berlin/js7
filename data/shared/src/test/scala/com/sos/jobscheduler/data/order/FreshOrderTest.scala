package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FreshOrderTest extends FreeSpec
{
  "JSON" in {
    testJson(
      FreshOrder(OrderId("ORDER-ID"), WorkflowPath("/WORKFLOW"), scheduledFor = None, Map.empty),
      json"""{
        "id": "ORDER-ID",
        "workflowPath": "/WORKFLOW"
      }""")

    testJson(
      FreshOrder(OrderId("ORDER-ID"), WorkflowPath("/WORKFLOW"), Some(Timestamp.parse("2017-03-07T12:00:00Z")), Map("KEY" -> "VALUE")),
      json"""{
        "id": "ORDER-ID",
        "workflowPath": "/WORKFLOW",
        "scheduledFor": 1488888000000,
        "arguments": {
          "KEY": "VALUE"
        }
      }""")
  }
}

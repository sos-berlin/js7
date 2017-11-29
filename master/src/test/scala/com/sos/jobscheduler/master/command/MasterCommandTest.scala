package com.sos.jobscheduler.master.command

import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.{NodeId, NodeKey, WorkflowPath}
import com.sos.jobscheduler.master.command.MasterCommand.Response._
import com.sos.jobscheduler.master.command.MasterCommand._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

final class MasterCommandTest extends FreeSpec {

  "Terminate" in {
    testJson[MasterCommand](Terminate,
      """{
        "TYPE": "Terminate"
      }""")
  }

  "AddOrderIfNew" in {
    testJson[MasterCommand](
      AddOrderIfNew(Order(OrderId("ORDER-ID"), NodeKey(WorkflowPath("/JOBNET"), NodeId("NODE-ID")), Order.StartNow,
        payload = Payload(Map("VAR" → "VALUE"), Outcome.Good(true)))),
      """{
        "TYPE": "AddOrderIfNew",
        "order": {
          "id": "ORDER-ID",
          "nodeKey": {
            "workflowPath": "/JOBNET",
            "nodeId": "NODE-ID"
          },
          "state": {
            "TYPE": "StartNow"
          },
          "payload": {
            "variables": {
              "VAR": "VALUE"
            },
            "outcome": {
              "TYPE": "Good",
              "returnValue": true
            }
          }
        }
      }""")
  }

  "ScheduleOrdersEvery" in {
    testJson[MasterCommand](
      ScheduleOrdersEvery(12345.ms.toFiniteDuration),
      """{
        "TYPE": "ScheduleOrdersEvery",
        "every": 12.345
       }""")
  }

  "Response.Accepted" in {
    testJson[MasterCommand.Response](
      Accepted,
      """{
        "TYPE": "Accepted"
      }""")
  }
}

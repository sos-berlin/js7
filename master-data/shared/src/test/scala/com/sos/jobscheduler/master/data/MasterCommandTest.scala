package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.data.MasterCommand._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import scala.concurrent.duration._

final class MasterCommandTest extends FreeSpec {

  "Terminate" in {
    testJson[MasterCommand](Terminate,
      """{
        "TYPE": "Terminate"
      }""")
  }

  "AddOrderIfNew" in {
    testJson[MasterCommand](
      AddOrderIfNew(Order(OrderId("ORDER-ID"), WorkflowPath("/JOBNET"), Order.StartNow,
        payload = Payload(Map("VAR" → "VALUE"), Outcome.Good(ReturnCode(0))))),
      """{
        "TYPE": "AddOrderIfNew",
        "order": {
          "id": "ORDER-ID",
          "workflowPosition": [ "/JOBNET", 0 ],
          "state": {
            "TYPE": "StartNow"
          },
          "payload": {
            "variables": {
              "VAR": "VALUE"
            },
            "outcome": {
              "TYPE": "Good",
              "returnCode": 0
            }
          }
        }
      }""")
  }

  "ScheduleOrdersEvery" in {
    testJson[MasterCommand](
      ScheduleOrdersEvery(12345.millis),
      """{
        "TYPE": "ScheduleOrdersEvery",
        "every": 12.345
       }""")
  }

  "Response.Accepted" in {
    testJson[MasterCommand.Response](
      Response.Accepted,
      """{
        "TYPE": "Accepted"
      }""")
  }
}

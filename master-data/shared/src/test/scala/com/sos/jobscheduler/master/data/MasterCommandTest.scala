package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.order.{Order, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.data.MasterCommand._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import scala.concurrent.duration._

final class MasterCommandTest extends FreeSpec {

  "Terminate" in {
    testJson[MasterCommand](Terminate,
      json"""{
        "TYPE": "Terminate"
      }""")
  }

  "AddOrderIfNew" in {
    testJson[MasterCommand](
      AddOrderIfNew(Order(OrderId("ORDER-ID"), WorkflowPath("/JOBNET"), Order.StartNow,
        payload = Payload(Map("VAR" → "VALUE")))),
      json"""{
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
            }
          }
        }
      }""")
  }

  "ScheduleOrdersEvery" in {
    testJson[MasterCommand](
      ScheduleOrdersEvery(12345.millis),
      json"""{
        "TYPE": "ScheduleOrdersEvery",
        "every": 12.345
       }""")
  }

  "ReadConfigurationDirectory" in {
    testJson[MasterCommand](ReadConfigurationDirectory,
      json"""{
        "TYPE": "ReadConfigurationDirectory"
      }""")
  }

  "Response.Accepted" in {
    testJson[MasterCommand.Response](
      Response.Accepted,
      json"""{
        "TYPE": "Accepted"
      }""")
  }
}

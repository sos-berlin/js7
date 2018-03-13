package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.order.{OrderId, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.data.MasterCommand._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import scala.concurrent.duration._

final class MasterCommandTest extends FreeSpec {

  "AddOrderIfNew" in {
    testJson[MasterCommand](
      AddOrderIfNew(OrderId("ORDER-ID"), WorkflowPath("/WORKFLOW"), scheduledAt = None, Payload.empty),
      json"""{
        "TYPE": "AddOrderIfNew",
        "id": "ORDER-ID",
        "workflowPath": "/WORKFLOW"
      }""")

    testJson[MasterCommand](
      AddOrderIfNew(OrderId("ORDER-ID"), WorkflowPath("/WORKFLOW"), Some(Timestamp.parse("2017-03-07T12:00:00Z")), Payload(Map("KEY" → "VALUE"))),
      json"""{
        "TYPE": "AddOrderIfNew",
        "id": "ORDER-ID",
        "workflowPath": "/WORKFLOW",
        "scheduledAt": 1488888000000,
        "variables": {
          "KEY": "VALUE"
        }
      }""")
  }

  "EmergencyStop" in {
    testJson[MasterCommand](EmergencyStop,
      json"""{
        "TYPE": "EmergencyStop"
      }""")
  }

  "ReadConfigurationDirectory" in {
    testJson[MasterCommand](ReadConfigurationDirectory(VersionId("VERSION")),
      json"""{
        "TYPE": "ReadConfigurationDirectory",
        "versionId": "VERSION"
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

  "Terminate" in {
    testJson[MasterCommand](Terminate,
      json"""{
        "TYPE": "Terminate"
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

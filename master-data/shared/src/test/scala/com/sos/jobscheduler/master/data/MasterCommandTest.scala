package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.master.data.MasterCommand._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import scala.concurrent.duration._

final class MasterCommandTest extends FreeSpec {

  "EmergencyStop" in {
    testJson[MasterCommand](EmergencyStop,
      json"""{
        "TYPE": "EmergencyStop"
      }""")
  }

  "KeepEvents" in {
    testJson[MasterCommand](
      KeepEvents(123),
      json"""{
        "TYPE": "KeepEvents",
        "after": 123
      }""")
  }

  "ReadConfigurationDirectory" - {
    "with versionId" in {
      testJson[MasterCommand](ReadConfigurationDirectory(Some(VersionId("VERSION"))),
        json"""{
          "TYPE": "ReadConfigurationDirectory",
          "versionId": "VERSION"
        }""")
    }

    "without versionId" in {
      testJson[MasterCommand](ReadConfigurationDirectory(None),
        json"""{
          "TYPE": "ReadConfigurationDirectory"
        }""")
    }
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

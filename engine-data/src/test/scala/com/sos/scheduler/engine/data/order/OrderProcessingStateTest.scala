package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.order.OrderProcessingState._
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderProcessingStateTest extends FreeSpec {

  "JSON" - {
    addTest(NotPlanned, """{
        "type": "NotPlanned"
      }""")
    addTest(Planned(Instant.parse("2016-08-01T11:22:33.444Z")),
      """{
        "type": "Planned",
        "at": "2016-08-01T11:22:33.444Z"
      }""")
    addTest(Pending(Instant.parse("2016-08-01T11:22:33.444Z")),
      """{
        "type": "Pending",
        "at": "2016-08-01T11:22:33.444Z"
      }""")
    addTest(WaitingInTask(TaskId(123)),
      """{
        "type": "WaitingInTask",
        "taskId": "123"
      }""")
    addTest(InTaskProcess(TaskId(123)),
      """{
        "type": "InTaskProcess",
        "taskId": "123"
      }""")
    addTest(Setback(Instant.parse("2016-08-01T11:22:33.444Z")),
      """{
        "type": "Setback",
        "at": "2016-08-01T11:22:33.444Z"
      }""")
    addTest(Blacklisted,
      """{
        "type": "Blacklisted"
      }""")
    addTest(Suspended,
      """{
        "type": "Suspended"
      }""")
    addTest(WaitingForOther,
      """{
        "type": "WaitingForOther"
      }""")
  }

  private def addTest(processingState: OrderProcessingState, json: String): Unit = {
    s"$processingState" in {
      val jsObject = json.parseJson.asJsObject
      assert(processingState.toJson == jsObject)
      assert(jsObject.convertTo[OrderProcessingState] == processingState)
    }
  }
}

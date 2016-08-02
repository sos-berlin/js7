package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.order.OrderProcessingState.NotPlanned
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
    "NotPlanned" in {
      val o: OrderProcessingState = NotPlanned
      val json = """{
        "type": "NotPlanned"
      }""".parseJson
      assert(o.toJson == json)
      assert(json.convertTo[OrderProcessingState] == o)
    }

    "Planned" in {
      val o: OrderProcessingState = OrderProcessingState.Planned(Instant.parse("2016-08-01T11:22:33.444Z"))
      val json = """{
        "type": "Planned",
        "at": "2016-08-01T11:22:33.444Z"
      }""".parseJson
      assert(o.toJson == json)
      assert(json.convertTo[OrderProcessingState] == o)
    }

    "Late" in {
      val o: OrderProcessingState = OrderProcessingState.Late(Instant.parse("2016-08-01T11:22:33.444Z"))
      val json = """{
        "type": "Late",
        "at": "2016-08-01T11:22:33.444Z"
      }""".parseJson
      assert(o.toJson == json)
      assert(json.convertTo[OrderProcessingState] == o)
    }

    "WaitingInTask" in {
      val o: OrderProcessingState = OrderProcessingState.WaitingInTask(TaskId(123))
      val json = """{
        "type": "WaitingInTask",
        "taskId": "123"
      }""".parseJson
      assert(o.toJson == json)
      assert(json.convertTo[OrderProcessingState] == o)
    }

    "InTaskProcess" in {
      val o: OrderProcessingState = OrderProcessingState.InTaskProcess(TaskId(123))
      val json = """{
        "type": "InTaskProcess",
        "taskId": "123"
      }""".parseJson
      assert(o.toJson == json)
      assert(json.convertTo[OrderProcessingState] == o)
    }

    "Blacklisted" in {
      val o: OrderProcessingState = OrderProcessingState.Blacklisted
      val json = """{
        "type": "Blacklisted"
      }""".parseJson
      assert(o.toJson == json)
      assert(json.convertTo[OrderProcessingState] == o)
    }

    "Suspended" in {
      val o: OrderProcessingState = OrderProcessingState.Suspended
      val json = """{
        "type": "Suspended"
      }""".parseJson
      assert(o.toJson == json)
      assert(json.convertTo[OrderProcessingState] == o)
    }

    "WaitingForOther" in {
      val o: OrderProcessingState = OrderProcessingState.WaitingForOther
      val json = """{
        "type": "WaitingForOther"
      }""".parseJson
      assert(o.toJson == json)
      assert(json.convertTo[OrderProcessingState] == o)
    }

  }
}

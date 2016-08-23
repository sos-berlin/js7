package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.agent.AgentAddress
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.order.OrderProcessingState._
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
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
    addTest(NotPlanned, """"NotPlanned"""")
    addTest(Planned(Instant.parse("2016-08-01T11:22:33.444Z")),
      """{
        "TYPE": "Planned",
        "at": "2016-08-01T11:22:33.444Z"
      }""")
    addTest(Pending(Instant.parse("2016-08-01T11:22:33.444Z")),
      """{
        "TYPE": "Pending",
        "at": "2016-08-01T11:22:33.444Z"
      }""")
    addTest(WaitingInTask(TaskId(123), ProcessClassPath("/TEST"), Some(AgentAddress("http://1.2.3.4:5678"))),
      """{
        "TYPE": "WaitingInTask",
        "taskId": "123",
        "processClassPath": "/TEST",
        "agentUri": "http://1.2.3.4:5678"
      }""")
    addTest(InTaskProcess(TaskId(123), ProcessClassPath("/TEST"), Some(AgentAddress("http://1.2.3.4:5678")), Instant.parse("2016-08-01T01:02:03.044Z")),
      """{
        "TYPE": "InTaskProcess",
        "taskId": "123",
        "processClassPath": "/TEST",
        "agentUri": "http://1.2.3.4:5678",
        "since": "2016-08-01T01:02:03.044Z"
      }""")
    addTest(Setback(Instant.parse("2016-08-01T11:22:33.444Z")),
      """{
        "TYPE": "Setback",
        "until": "2016-08-01T11:22:33.444Z"
      }""")
    addTest(Blacklisted, """"Blacklisted"""")
    addTest(WaitingForOther, """"WaitingForOther"""")
  }

  private def addTest(processingState: OrderProcessingState, json: String): Unit = {
    s"$processingState" in {
      val jsValue = json.parseJson
      assert(processingState.toJson == jsValue)
      assert(jsValue.convertTo[OrderProcessingState] == processingState)
    }
  }
}

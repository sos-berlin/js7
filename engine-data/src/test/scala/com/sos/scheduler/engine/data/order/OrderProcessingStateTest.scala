package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.agent.AgentAddress
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.order.OrderProcessingState._
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import com.sos.scheduler.engine.data.scheduler.ClusterMemberId
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
    addTest(NotPlanned,
      """{
        "TYPE": "NotPlanned"
      }""")
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
    addTest(WaitingInTask(TaskId(123), ProcessClassPath("/TEST")),
      """{
        "TYPE": "WaitingInTask",
        "taskId": "123",
        "processClassPath": "/TEST"
      }""")
    addTest(InTaskProcess(TaskId(123), ProcessClassPath("/TEST"),
      Instant.parse("2016-08-01T01:02:03.044Z"), Some(AgentAddress("http://1.2.3.4:5678"))),
      """{
        "TYPE": "InTaskProcess",
        "taskId": "123",
        "processClassPath": "/TEST",
        "since": "2016-08-01T01:02:03.044Z",
        "agentUri": "http://1.2.3.4:5678"
      }""")
    addTest(OccupiedByClusterMember(ClusterMemberId("CLUSTER-MEMBER-ID")),
      """{
        "TYPE": "OccupiedByClusterMember",
        "clusterMemberId": "CLUSTER-MEMBER-ID"
      }""")
    addTest(Setback(Instant.parse("2016-08-01T11:22:33.444Z")),
      """{
        "TYPE": "Setback",
        "until": "2016-08-01T11:22:33.444Z"
      }""")
    addTest(Blacklisted,
      """{
        "TYPE": "Blacklisted"
      }""")
    addTest(WaitingForOther,
      """{
        "TYPE": "WaitingForOther"
      }""")
  }

  private def addTest(processingState: OrderProcessingState, json: String): Unit = {
    s"$processingState" in {
      val jsValue = json.parseJson
      assert(processingState.toJson == jsValue)
      assert(jsValue.convertTo[OrderProcessingState] == processingState)
    }
  }
}

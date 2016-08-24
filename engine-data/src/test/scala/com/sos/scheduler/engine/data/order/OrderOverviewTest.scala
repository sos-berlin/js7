package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.agent.AgentAddress
import com.sos.scheduler.engine.data.filebased.FileBasedState
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.jobchain.{JobChainPath, NodeId}
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable.ListSet
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderOverviewTest extends FreeSpec {

  "JSON" in {
    val orderKey = JobChainPath("/a") orderKey "1"
    val obj = OrderOverview(
      orderKey,
      FileBasedState.active,
      OrderSourceType.AdHoc,
      NodeId("100"),
      OrderProcessingState.InTaskProcess(TaskId(123), ProcessClassPath("/TEST"), Some(AgentAddress("http://1.2.3.4:5678")), Instant.parse("2016-08-01T01:02:03.044Z")),
      ListSet(OrderObstacle.Suspended, OrderObstacle.Setback(Instant.parse("2016-08-02T11:22:33.444Z"))),
      nextStepAt = Some(Instant.parse("2016-07-18T12:00:00Z")),
      liveChanged = Some(OrderOverview.Replaced(OrderOverview(
        orderKey,
        FileBasedState.initialized,
        OrderSourceType.Permanent,
        NodeId(""),
        OrderProcessingState.Pending(Instant.ofEpochMilli(0))))))
    val jsValue = """{
      "path": "/a,1",
      "fileBasedState": "active",
      "sourceType": "AdHoc",
      "nodeId": "100",
      "processingState": {
        "TYPE": "InTaskProcess",
        "taskId": "123",
        "since": "2016-08-01T01:02:03.044Z",
        "processClassPath": "/TEST",
        "agentUri": "http://1.2.3.4:5678"
      },
      "obstacles": [
        {
          "TYPE": "Suspended"
        },
        {
          "TYPE": "Setback",
          "until": "2016-08-02T11:22:33.444Z"
        }
      ],
      "nextStepAt": "2016-07-18T12:00:00Z",
      "liveChanged": {
        "TYPE": "Replaced",
        "overview": {
          "sourceType": "Permanent",
          "path": "/a,1",
          "processingState": {
            "at": "1970-01-01T00:00:00Z",
            "TYPE": "Pending"
          },
          "fileBasedState": "initialized",
          "obstacles": [],
          "nodeId":""
        }
      }
    }""".parseJson
    assert(obj.toJson == jsValue)
    assert(obj == jsValue.convertTo[OrderOverview])
  }
}

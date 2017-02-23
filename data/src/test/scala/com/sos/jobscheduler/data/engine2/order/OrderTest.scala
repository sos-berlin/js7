package com.sos.jobscheduler.data.engine2.order

import com.sos.jobscheduler.data.order.OrderId
import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class OrderTest extends FreeSpec {

  "JSON InitialOutcome" in {
    check(
      Order(
        OrderId("ID"),
        NodeKey(JobChainPath("/JOBCHAIN"), NodeId("NODE")),
        Order.InProcess,
        Map(
          "var1" → "value1",
          "var2" → "value2"),
        Order.InitialOutcome),
      """{
        "id": "ID",
        "nodeKey": {
          "jobChainPath": "/JOBCHAIN",
          "nodeId": "NODE"
        },
        "state": {
          "TYPE": "InProcess"
        },
        "variables": {
          "var1": "value1",
          "var2": "value2"
        },
        "outcome": {
          "returnValue": true
        }
      }""")
  }

  "JSON Bad" in {
    check(
      Order(
        OrderId("ID"),
        NodeKey(JobChainPath("/JOBCHAIN"), NodeId("NODE")),
        Order.Waiting,
        Map(),
        Order.Bad("MESSAGE")),
      """{
        "id": "ID",
        "nodeKey": {
          "jobChainPath": "/JOBCHAIN",
          "nodeId": "NODE"
        },
        "state": {
          "TYPE": "Waiting"
        },
        "variables": {},
        "outcome": {
          "error": "MESSAGE"
        }
      }""")
  }

  private def check(o: Order[Order.State], json: String): Unit = {
    assert(o.toJson == json.parseJson)
    assert(json.parseJson.convertTo[Order[Order.State]] == o)
  }
}

package com.sos.jobscheduler.master.gui.data

import io.circe.parser.parse
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderTest extends FreeSpec {

  "JSON InitialOutcome" in {
    check(
      Order(
        OrderId("ID"),
        NodeKey(JobnetPath("/JOBNET"), NodeId("NODE")),
        Order.InProcess,
        Map(
          "var1" → "value1",
          "var2" → "value2"),
        Order.InitialOutcome),
      """{
        "id": "ID",
        "nodeKey": {
          "jobnetPath": "/JOBNET",
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
          "TYPE": "Good",
          "returnValue": true
        }
      }""")
  }

  "JSON Bad" in {
    check(
      Order(
        OrderId("ID"),
        NodeKey(JobnetPath("/JOBNET"), NodeId("NODE")),
        Order.Ready,
        Map(),
        Order.Bad("MESSAGE")),
      """{
        "id": "ID",
        "nodeKey": {
          "jobnetPath": "/JOBNET",
          "nodeId": "NODE"
        },
        "state": {
          "TYPE": "Ready"
        },
        "variables": {},
        "outcome": {
          "TYPE": "Bad",
          "error": "MESSAGE"
        }
      }""")
  }

  private def check(o: Order[Order.State], json: String): Unit = {
    //assert(o.asJson == parse(json).toTry.get)
    assert(parse(json).toTry.get.as[Order[Order.State]] == Right(o))
  }
}

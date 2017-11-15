package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.data.jobnet.{JobnetPath, NodeId, NodeKey}
import java.time.Instant
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
          "returnValue": true
        }
      }""")
  }

  "JSON Bad" in {
    check(
      Order(
        OrderId("ID"),
        NodeKey(JobnetPath("/JOBNET"), NodeId("NODE")),
        Order.Waiting,
        Map(),
        Order.Bad("MESSAGE")),
      """{
        "id": "ID",
        "nodeKey": {
          "jobnetPath": "/JOBNET",
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

  "Order.State" - {
    import Order._

    "Scheduled" in {
      check(Scheduled(Instant.parse("2017-11-15T12:33:44.789Z")),
        """{
           "TYPE": "Scheduled",
           "at": 1510749224789
        }""")
    }

    "StartNow" in {
      check(StartNow,
        """{
           "TYPE": "StartNow"
        }""")
    }

    "Waiting" in {
      check(Waiting,
        """{
           "TYPE": "Waiting"
        }""")
    }

    "Ready" in {
      check(Ready,
        """{
           "TYPE": "Ready"
        }""")
    }

    "InProcess" in {
      check(InProcess,
        """{
           "TYPE": "InProcess"
        }""")
    }

    "Detached" in {
      check(Detached,
        """{
           "TYPE": "Detached"
        }""")
    }

    "Finished" in {
      check(Finished,
        """{
           "TYPE": "Finished"
        }""")
    }
  }

  private def check(o: Order[Order.State], json: String): Unit = {
    assert(o.toJson == json.parseJson)
    assert(json.parseJson.convertTo[Order[Order.State]] == o)
  }

  private def check(o: Order.State, json: String): Unit = {
    assert(o.toJson == json.parseJson)
    assert(json.parseJson.convertTo[Order.State] == o)
  }
}

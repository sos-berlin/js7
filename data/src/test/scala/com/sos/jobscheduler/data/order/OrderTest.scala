package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.data.jobnet.{JobnetPath, NodeId, NodeKey}
import java.time.Instant
import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class OrderTest extends FreeSpec {

  "Outcome" - {
    "Outcome" in {
      assert(Order.Good(returnValue = true).isSuccess)
      assert(!Order.Good(returnValue = false).isSuccess)
      assert(!Order.Bad(Order.Bad.Other("error")).isSuccess)
      assert(Order.Bad(Order.Bad.Other("error")) == Order.Bad("error"))
      assert(!Order.Bad(Order.Bad.AgentAborted).isSuccess)
    }

    "JSON" - {
      "Good" in {
         check(Order.Good(true), """
            {
              "TYPE": "Good",
              "returnValue": true
            }""".stripMargin)
      }

      "Bad AgentAborted" in {
         check(Order.Bad(Order.Bad.AgentAborted), """
            {
              "TYPE": "Bad",
              "reason": {
                "TYPE": "AgentAborted"
              }
            }""".stripMargin)
      }

      "Bad Other" in {
         check(Order.Bad("OTHER REASON"), """
            {
              "TYPE": "Bad",
              "reason": {
                "TYPE": "Other",
                "message": "OTHER REASON"
              }
            }""".stripMargin)
      }

      def check(o: Order.Outcome, json: String): Unit = {
        assert(o.toJson == json.parseJson)
        assert(json.parseJson.convertTo[Order.Outcome] == o)
      }
    }
  }

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

    "Processed" in {
      check(Processed,
        """{
           "TYPE": "Processed"
        }""")
    }

    "Detachable" in {
      check(Detachable,
        """{
           "TYPE": "Detachable"
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

package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.workflow.{NodeId, NodeKey, WorkflowPath}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderTest extends FreeSpec {

  "Order" - {
    val order = Order(
      OrderId("ID"),
      NodeKey(WorkflowPath("/JOBNET"), NodeId("NODE")),
      Order.Ready,
      payload = Payload(Map(
        "var1" → "value1",
        "var2" → "value2")))

    "OrderDetachable" in {
      intercept[IllegalStateException] {
        order.update(OrderEvent.OrderDetachable)
      }
    }

    "attachedToAgent" in {
      val agentPath = AgentPath("/A")
      assert(order.attachedToAgent.isLeft)
      assert(order.copy(attachedTo = Some(Order.AttachedTo.Agent(agentPath)))     .attachedToAgent == Right(agentPath))
      assert(order.copy(attachedTo = Some(Order.AttachedTo.Detachable(agentPath))).attachedToAgent.isLeft)
    }

    "detachableFromAgent" in {
      val agentPath = AgentPath("/A")
      assert(order.detachableFromAgent.isLeft)
      assert(order.copy(attachedTo = Some(Order.AttachedTo.Agent(agentPath)))     .detachableFromAgent.isLeft)
      assert(order.copy(attachedTo = Some(Order.AttachedTo.Detachable(agentPath))).detachableFromAgent == Right(agentPath))
    }

    "castState" in {
      assert(order.castState[Order.Ready.type] eq order)
      assert(order.castState[Order.Idle] eq order)
      assert(order.castState[Order.State] eq order)
      intercept[IllegalStateException] {
        order.castState[Order.Processed.type]
      }
    }

    "ifState" in {
      assert(order.ifState[Order.Ready.type] == Some(order))
      assert(order.ifState[Order.Idle] == Some(order))
      assert(order.ifState[Order.State] == Some(order))
      assert(order.ifState[Order.Processed.type] == None)
    }

    "JSON" in {
      check(
        order.copy(
          attachedTo = Some(Order.AttachedTo.Agent(AgentPath("/AGENT"))),
          parent = Some(OrderId("PARENT"))),
        """{
          "id": "ID",
          "nodeKey": {
            "workflowPath": "/JOBNET",
            "nodeId": "NODE"
          },
          "state": {
            "TYPE": "Ready"
          },
          "attachedTo": {
            "TYPE": "Agent",
            "agentPath": "/AGENT"
          },
          "parent": "PARENT",
          "payload": {
            "variables": {
              "var1": "value1",
              "var2": "value2"
            },
            "outcome": {
              "TYPE": "Good",
              "returnValue": true
            }
          }
        }""")

      def check(o: Order[Order.State], json: String) = testJson(o, json)
    }
  }

  "State" - {
    "Scheduled" in {
      check(Scheduled(Timestamp.parse("2017-11-15T12:33:44.789Z")),
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

    "Forked" in {
      check(Forked(List(OrderId("A/1"), OrderId("A/2"))),
        """{
           "TYPE": "Forked",
           "childOrderIds": [ "A/1", "A/2" ]
        }""")
    }

    "Finished" in {
      check(Finished,
        """{
           "TYPE": "Finished"
        }""")
    }

    def check(o: Order.State, json: String) = testJson(o, json)
  }

  "isAttachable" in {
    val order = Order(OrderId("ORDER-ID"), NodeKey(WorkflowPath("/JOBNET"), NodeId("NODE")), Order.Ready, Some(AttachedTo.Detachable(AgentPath("/AGENT"))))
    assert(order.detachableFromAgent == Right(AgentPath("/AGENT")))

    for (o ← Array(
          order.copy(attachedTo = Some(Order.AttachedTo.Agent(AgentPath("/AGENT")))),
          order.copy(attachedTo = None))) {
      val e: IllegalStateException = o.detachableFromAgent.left.get
      assert(e.getMessage contains "ORDER-ID")
    }
  }

  "AttachedTo" - {
    "Agent" in {
      check(AttachedTo.Agent(AgentPath("/AGENT")),
        """{
          "TYPE": "Agent",
          "agentPath": "/AGENT"
        }""")
    }

    "Detachable" in {
      check(AttachedTo.Detachable(AgentPath("/AGENT")),
        """{
          "TYPE": "Detachable",
          "agentPath": "/AGENT"
        }""")
    }

    def check(o: Order.AttachedTo, j: String) = testJson(o, j)
  }
}

package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.Position
import com.sos.jobscheduler.data.workflow.test.TestSetting.TestWorkflow
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
final class AgentCommandTest extends FreeSpec {

  "Batch" in {
    check(AgentCommand.Batch(List(AgentCommand.NoOperation, AgentCommand.Logout)),
      """{
        "TYPE": "Batch",
        "commands": [
          { "TYPE": "NoOperation" },
          { "TYPE": "Logout" }
        ]
      }""")
  }

  "AbortImmediately" in {
    check(AgentCommand.AbortImmediately,
      """{ "TYPE": "AbortImmediately" }""")
  }

  "Login" in {
    check(AgentCommand.Login,
      """{
        "TYPE": "Login"
      }""")
  }

  "Logout" in {
    check(AgentCommand.Logout,
      """{
        "TYPE": "Logout"
      }""")
  }

  "NoOperation" in {
    check(AgentCommand.NoOperation,
      """{
        "TYPE": "NoOperation"
      }""")
  }

  "RegisterAsMaster" in {
    check(AgentCommand.RegisterAsMaster,
      """{
        "TYPE": "RegisterAsMaster"
      }""")
  }

  "Terminate" - {
    "JSON without sigkillProcessesAfter" in {
      check(AgentCommand.Terminate(sigtermProcesses = true),
        """{
          "TYPE":"Terminate",
          "sigtermProcesses": true
        }""")
    }

    "JSON with sigkillProcessesAfter" in {
      check(AgentCommand.Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(30.seconds)),
        """{
          "TYPE":"Terminate",
          "sigtermProcesses": true,
          "sigkillProcessesAfter": 30
        }""")
    }
  }

  "OrderCommand" - {
    "AttachOrder" in {
      check(AgentCommand.AttachOrder(
        Order(
          OrderId("ORDER-ID"),
          TestWorkflow.path /: Position(3),
          Order.Ready,
          Some(Order.AttachedTo.Agent(AgentPath("/AGENT")))),
        AgentPath("/AGENT"),
        TestWorkflow.workflow),
        """{
          "TYPE": "AttachOrder",
          "order": {
            "state": {
              "TYPE": "Ready"
            },
            "payload": {
              "variables": {}
            },
            "attachedTo": {
              "agentPath": "/AGENT",
              "TYPE": "Agent"
            },
            "id": "ORDER-ID",
            "workflowPosition": [ "/WORKFLOW", 3 ]
          },
          "workflow": {
            "instructions": [
              { "TYPE": "Job", "job": { "agentPath": "/AGENT", "jobPath": "/A" } },
              { "TYPE": "Job", "job": { "agentPath": "/AGENT", "jobPath": "/B" } },
              { "TYPE": "ImplicitEnd" }
            ]
          }
        }""")
    }

    "DetachOrder" in {
      check(AgentCommand.DetachOrder(OrderId("ORDER-ID")),
        """{
          "TYPE": "DetachOrder",
          "orderId": "ORDER-ID"
        }""")
    }

    "GetOrder" in {
      check(AgentCommand.GetOrder(OrderId("ORDER-ID")),
         """{
          "TYPE": "GetOrder",
          "orderId": "ORDER-ID"
        }""")
    }

    "GetOrderIds" in {
      check(AgentCommand.GetOrderIds,
        """{
          "TYPE": "GetOrderIds"
        }""")
    }
  }

  private def check(command: AgentCommand, jsonString: String): Unit =
    testJson(command, jsonString)
}

package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.base.process.ProcessSignal.SIGTERM
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.TaskId
import com.sos.jobscheduler.data.jobnet.Jobnet.{EndNode, JobNode}
import com.sos.jobscheduler.data.jobnet.{JobPath, Jobnet, JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import java.time.Duration
import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class AgentCommandTest extends FreeSpec {

  "AbortImmediately" in {
    val obj = AgentCommand.AbortImmediately
    val json = """{ "$TYPE": "AbortImmediately" }""".parseJson
    assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
    assert(obj == json.convertTo[AgentCommand])
  }

  "Login" in {
    val obj = AgentCommand.Login
    val json = """{
      "$TYPE": "Login"
    }""".parseJson
    assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
    assert(obj == json.convertTo[AgentCommand.Login.type])
  }

  "Logout" in {
    val obj = AgentCommand.Logout
    val json = """{
      "$TYPE": "Logout"
    }""".parseJson
    assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
    assert(obj == json.convertTo[AgentCommand.Logout.type])
  }

  "NoOperation" in {
    val obj = AgentCommand.NoOperation
    val json = """{
      "$TYPE": "NoOperation"
    }""".parseJson
    assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
    assert(obj == json.convertTo[AgentCommand.NoOperation.type])
  }

  "RegisterAsMaster" in {
    val obj = AgentCommand.RegisterAsMaster
    val json = """{
      "$TYPE": "RegisterAsMaster"
    }""".parseJson
    assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
    assert(obj == json.convertTo[AgentCommand.RegisterAsMaster.type])
  }

  "Terminate" - {
    "JSON without sigkillProcessesAfter" in {
      val obj = AgentCommand.Terminate(sigtermProcesses = true)
      val json = """{
        "$TYPE":"Terminate",
        "sigtermProcesses": true
      }""".parseJson
      assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
      assert(obj == json.convertTo[AgentCommand])
    }

    "JSON with sigkillProcessesAfter" in {
      val obj = AgentCommand.Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(30.s))
      val json = """{
        "$TYPE":"Terminate",
        "sigtermProcesses": true,
        "sigkillProcessesAfter": 30
      }""".parseJson
      //assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
      assert(obj == json.convertTo[AgentCommand])
    }
  }

  "OrderCommand" - {
    "AttachJobnet" in {
      val obj = AgentCommand.AttachJobnet(Jobnet(
        JobnetPath("/JOBNET"),
        NodeId("START"),
        List(
          JobNode(NodeId("START"), AgentPath("/AGENT"), JobPath("/JOB"), NodeId("END"), NodeId("END")),
          EndNode(NodeId("END")))))
      val json = """{
        "$TYPE": "AttachJobnet",
        "jobnet": {
          "path": "/JOBNET",
          "inputNodeId": "START",
          "idToNode": {
            "START": {
              "agentPath": "/AGENT",
              "onSuccess": "END",
              "id": "START",
              "jobPath": "/JOB",
              "onFailure": "END",
              "TYPE": "JobNode"
            },
            "END": {
              "id": "END",
              "TYPE": "EndNode"
            }
          }
        }
      }""".parseJson
      assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
      assert(obj == json.convertTo[AgentCommand])
    }

    "AttachOrder" in {
      val obj = AgentCommand.AttachOrder(Order(
        OrderId("ORDER-ID"),
        NodeKey(JobnetPath("/JOBNET"),NodeId("INPUT")),
        Order.Waiting))
      val json = """{
        "$TYPE": "AttachOrder",
        "order": {
          "state": {
            "TYPE":
            "Waiting"
          },
          "outcome": {
            "returnValue": true
          },
          "variables": {},
          "id": "ORDER-ID",
          "nodeKey": {
            "jobnetPath": "/JOBNET",
            "nodeId": "INPUT"
          }
        }
      }""".parseJson
      assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
      assert(obj == json.convertTo[AgentCommand])
    }

    "DetachOrder" in {
      val obj = AgentCommand.DetachOrder(OrderId("ORDER-ID"))
      val json = """{
        "$TYPE": "DetachOrder",
        "orderId": "ORDER-ID"
      }""".parseJson
      assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
      assert(obj == json.convertTo[AgentCommand])
    }

    "GetOrder" in {
      val obj = AgentCommand.GetOrder(OrderId("ORDER-ID"))
      val json = """{
        "$TYPE": "GetOrder",
        "orderId": "ORDER-ID"
      }""".parseJson
      assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
      assert(obj == json.convertTo[AgentCommand])
    }

    "GetOrderIds" in {
      val obj = AgentCommand.GetOrderIds
      val json = """{
        "$TYPE": "GetOrderIds"
      }""".parseJson
      assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
      assert(obj == json.convertTo[AgentCommand])
    }
  }

  "Legacy version 1 commands" - {
    "CloseTask" in {
      val obj = AgentCommand.CloseTask(AgentTaskId("111-222"), kill = true)
      val json = """{
        "$TYPE": "CloseTask",
        "agentTaskId": "111-222",
        "kill": true
      }""".parseJson
      assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
      assert(obj == json.convertTo[AgentCommand])
    }

    "DeleteFile" in {
      val obj = AgentCommand.DeleteFile("FILE")
      val json = """{
        "$TYPE": "DeleteFile",
        "path": "FILE"
      }""".parseJson
      assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
      assert(obj == json.convertTo[AgentCommand])
    }

    "MoveFile" in {
      val obj = AgentCommand.MoveFile("OLD", "TO-DIRECTORY")
      val json = """{
        "$TYPE": "MoveFile",
        "path": "OLD",
        "toDirectory": "TO-DIRECTORY"
      }""".parseJson
      assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
      assert(obj == json.convertTo[AgentCommand])
    }

    "RequestFileOrderSourceContent" in {
      val obj = AgentCommand.RequestFileOrderSourceContent(
        directory = "DIRECTORY",
        regex = "REGEX",
        duration = Duration.ofMillis(111222333444555666L),
        knownFiles = Set("KNOWN"))
      val json = """{
        "$TYPE": "RequestFileOrderSourceContent",
        "directory": "DIRECTORY",
        "regex": "REGEX",
        "duration": 111222333444555.666,
        "knownFiles": [ "KNOWN" ]
      }""".parseJson
      assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
      assert(obj == json.convertTo[AgentCommand])
    }

    "SendProcessSignal" in {
      val obj = AgentCommand.SendProcessSignal(AgentTaskId("111-222"), SIGTERM)
      val json = """{
        "$TYPE": "SendProcessSignal",
        "agentTaskId": "111-222",
        "signal": "SIGTERM"
      }""".parseJson
      assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
      assert(obj == json.convertTo[AgentCommand])
    }

    "StartApiTask" - {
      "JSON minimum" in {
        val obj = AgentCommand.StartApiTask(
          meta = None,
          javaOptions = "JAVA-OPTIONS",
          javaClasspath = "JAVA-CLASSPATH")
        val json = """{
          "$TYPE": "StartApiTask",
          "javaOptions": "JAVA-OPTIONS",
          "javaClasspath": "JAVA-CLASSPATH"
        }""".parseJson
        assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
        assert(obj == json.convertTo[AgentCommand])
      }

      "JSON maximum" in {
        val obj = AgentCommand.StartApiTask(
          meta = Some(AgentCommand.StartTask.Meta(
            job = "/folder/test",
            TaskId(123))),
          javaOptions = "JAVA-OPTIONS",
          javaClasspath = "JAVA-CLASSPATH")
        val json = """{
          "$TYPE": "StartApiTask",
          "meta": {
            "job": "/folder/test",
            "taskId": "123"
          },
          "javaOptions": "JAVA-OPTIONS",
          "javaClasspath": "JAVA-CLASSPATH"
        }""".parseJson
        assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
        assert(obj == json.convertTo[AgentCommand])
      }
    }

    "StartNonApiTask" - {
      "JSON minimum" in {
        val obj = AgentCommand.StartNonApiTask(meta = None)
        val json = """{ "$TYPE": "StartNonApiTask" }""".parseJson
        assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
        assert(obj == json.convertTo[AgentCommand])
      }

      "JSON maximum" in {
        val obj = AgentCommand.StartNonApiTask(Some(AgentCommand.StartTask.Meta(job = "/folder/test", TaskId(123))))
        val json =
          """{
            "$TYPE": "StartNonApiTask",
            "meta": {
              "job": "/folder/test",
              "taskId": "123"
              }
            }""".parseJson
        assert((obj: AgentCommand).toJson == json)   // AgentCommand serializer includes $TYPE
        assert(obj == json.convertTo[AgentCommand])
      }
    }
  }
}

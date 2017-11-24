package com.sos.jobscheduler.master.command

import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.{NodeId, NodeKey, WorkflowPath}
import com.sos.jobscheduler.master.command.MasterCommand.Response._
import com.sos.jobscheduler.master.command.MasterCommand._
import org.scalatest.FreeSpec
import spray.json._

final class MasterCommandTest extends FreeSpec {

  "Terminate" in {
    check(Terminate,
      """{
        "TYPE": "Terminate"
      }""".parseJson)
  }

  "AddOrderIfNew" in {
    check(AddOrderIfNew(Order(OrderId("ORDER-ID"), NodeKey(WorkflowPath("/JOBNET"), NodeId("NODE-ID")), Order.Ready, Map("VAR" → "VALUE"), Order.Good(true))),
      """{
        "TYPE": "AddOrderIfNew",
        "order": {
          "id": "ORDER-ID",
           "nodeKey": {
             "workflowPath": "/JOBNET",
             "nodeId": "NODE-ID"
           },
           "state": {
             "TYPE": "Ready"
           },
           "variables": {
             "VAR": "VALUE"
           },
           "outcome": {
             "TYPE": "Good",
             "returnValue": true
           }
         }
       }""".parseJson)
  }

  "ScheduleOrdersEvery" in {
    check(ScheduleOrdersEvery(12345.ms),
      """{
        "TYPE": "ScheduleOrdersEvery",
        "every": 12.345
       }""".parseJson)
  }

  "Response.Accepted" in {
    check(Accepted,
      """{
        "TYPE": "Accepted"
      }""".parseJson)
  }

  private def check(command: MasterCommand, json: JsValue): Unit = {
    assert(command.toJson == json)
    assert(command == json.convertTo[MasterCommand])
  }

  private def check(response: Response, json: JsValue): Unit = {
    assert(response.toJson == json)
    assert(response == json.convertTo[Response])
  }
}

package com.sos.jobscheduler.master.command

import MasterCommand._
import org.scalatest.FreeSpec
import spray.json._
import Response._
import com.sos.jobscheduler.data.jobnet.{JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.common.time.ScalaTime._

final class MasterCommandTest extends FreeSpec {

  "Terminate" in {
    check(Terminate,
      """{
        "TYPE": "Terminate"
      }""".parseJson)
  }

  "AddOrderIfNew" in {
    check(AddOrderIfNew(Order(OrderId("ORDER-ID"), NodeKey(JobnetPath("/JOBNET"), NodeId("NODE-ID")), Order.Waiting, Map("VAR" → "VALUE"), Order.Good(true))),
      """{
        "TYPE": "AddOrderIfNew",
        "order": {
          "id": "ORDER-ID",
           "nodeKey": {
             "jobnetPath": "/JOBNET",
             "nodeId": "NODE-ID"
           },
           "state": {
             "TYPE": "Waiting"
           },
           "variables": {
             "VAR": "VALUE"
           },
           "outcome": {
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

package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderEventTest extends FreeSpec {

  private val orderKey = JobChainPath("/JOB-CHAIN") orderKey "ORDER-ID"

  "OrderFinished" in {
    check(OrderFinished(orderKey, OrderState("END")),
      """{
        "TYPE": "OrderFinished",
        "orderKey": "/JOB-CHAIN,ORDER-ID",
        "state": "END"
      }""")
  }

  "OrderNestedFinished" in {
    check(OrderNestedFinished(orderKey),
      """{
        "TYPE": "OrderNestedFinished",
        "orderKey": "/JOB-CHAIN,ORDER-ID"
      }""")
    }

  "OrderNestedStarted" in {
    check(OrderNestedStarted(orderKey),
      """{
        "TYPE": "OrderNestedStarted",
        "orderKey": "/JOB-CHAIN,ORDER-ID"
      }""")
  }

  "OrderResumed" in {
    check(OrderResumed(orderKey),
      """{
        "TYPE": "OrderResumed",
        "orderKey": "/JOB-CHAIN,ORDER-ID"
      }""")
  }

  "OrderSetBack" in {
    check(OrderSetBack(orderKey, OrderState("100")),
      """{
        "TYPE": "OrderSetBack",
        "orderKey": "/JOB-CHAIN,ORDER-ID",
        "state": "100"
      }""")
  }

  "OrderNodeChanged" in {
    check(OrderNodeChanged(orderKey, OrderState("50"), OrderState("100")),
      """{
        "TYPE": "OrderNodeChanged",
        "orderKey": "/JOB-CHAIN,ORDER-ID",
        "state": "100",
        "previousState": "50"
      }""")
  }

  "OrderStepEnded" in {
    check(OrderStepEnded(orderKey, OrderNodeTransition.Success),
      """{
        "TYPE": "OrderStepEnded",
        "orderKey": "/JOB-CHAIN,ORDER-ID",
        "stateTransition": "Success"
      }""")
  }

  "OrderStepStarted" in {
    check(OrderStepStarted(orderKey, OrderState("100"), TaskId(123)),
      """{
        "TYPE": "OrderStepStarted",
        "orderKey": "/JOB-CHAIN,ORDER-ID",
        "state": "100",
        "taskId": "123"
      }""")
  }

  "OrderSuspended" in {
    check(OrderSuspended(orderKey),
      """{
        "TYPE": "OrderSuspended",
        "orderKey": "/JOB-CHAIN,ORDER-ID"
      }""")
    }

  "OrderStarted" in {
    check(OrderStarted(orderKey),
      """{
        "TYPE": "OrderStarted",
        "orderKey": "/JOB-CHAIN,ORDER-ID"
      }""")
    }

  private def check(event: OrderEvent, json: String): Unit = {
    val jsValue = json.parseJson
    assert (event.toJson == jsValue)
    assert (event == jsValue.convertTo[OrderEvent] )
  }
}

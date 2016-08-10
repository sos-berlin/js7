package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.job.ReturnCode
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class OrderNodeTransitionTest extends FreeSpec {

  "0 is success" in {
    OrderNodeTransition.ofCppInternalValue(0) shouldEqual OrderNodeTransition.Success
  }

  "other is error" in {
    for (i ← -300 to 300; if i != 0)
      OrderNodeTransition.ofCppInternalValue(i) shouldEqual OrderNodeTransition.Error(ReturnCode(i))
  }

  "keep state" in {
    OrderNodeTransition.ofCppInternalValue(Long.MaxValue) shouldEqual OrderNodeTransition.Keep
  }

  "invalid values" in {
    intercept[AssertionError] { OrderNodeTransition.ofCppInternalValue(Int.MaxValue.toLong + 1) }
    intercept[AssertionError] { OrderNodeTransition.ofCppInternalValue(Int.MinValue.toLong - 1) }
  }

  "JSON" - {
    "Success" in {
      check(OrderNodeTransition.Success, """"Success"""")
    }

    "Error" in {
      check(OrderNodeTransition.Error(ReturnCode(99)), """{
        "TYPE": "Error",
        "returnCode": 99
      }""")
    }

    "Proceeding 0" in {
      check(OrderNodeTransition.Proceeding(ReturnCode(0)), """"Success"""")
    }

    "Proceeding 1" in {
      check(OrderNodeTransition.Proceeding(ReturnCode(1)), """{
        "TYPE": "Error",
        "returnCode": 1
      }""")
    }

    "Keep" in {
      check(OrderNodeTransition.Keep, """"Keep"""")
    }
  }

  private def check(event: OrderNodeTransition, json: String): Unit = {
    val jsValue = json.parseJson
    assert (event.toJson == jsValue)
    assert (event == jsValue.convertTo[OrderNodeTransition] )
  }
}

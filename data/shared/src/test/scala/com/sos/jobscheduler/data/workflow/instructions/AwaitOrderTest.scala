package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AwaitOrderTest extends AnyFreeSpec {

  "JSON" - {
    "with defaults" in {
      testJson[Instruction.Labeled](
        AwaitOrder(OrderId("ORDER")),
        json"""{
          "TYPE": "AwaitOrder",
          "orderId": "ORDER"
        }""")
    }

    "complete" in {
      testJson[Instruction.Labeled](
        AwaitOrder(OrderId("ORDER"), Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "AwaitOrder",
          "orderId": "ORDER",
          "sourcePos": [ 1, 2 ]
        }""")
    }
  }
}

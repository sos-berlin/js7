package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.data.order.OrderId
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
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

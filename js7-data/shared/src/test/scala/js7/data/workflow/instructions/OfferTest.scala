package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.base.time.ScalaTime._
import js7.data.order.OrderId
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OfferTest extends AnyFreeSpec {

  "JSON" in {
    testJson[Instruction.Labeled](
      Offer(OrderId("OFFERED"), 60.s, Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "Offer",
        "orderId": "OFFERED",
        "timeout": 60,
        "sourcePos": [ 1, 2 ]
      }""")
  }
}

package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import scala.concurrent.duration._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OfferTest extends AnyFreeSpec {

  "JSON" in {
    testJson[Instruction.Labeled](
      Offer(OrderId("OFFERED"), 60.seconds, Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "Offer",
        "orderId": "OFFERED",
        "timeout": 60,
        "sourcePos": [ 1, 2 ]
      }""")
  }
}

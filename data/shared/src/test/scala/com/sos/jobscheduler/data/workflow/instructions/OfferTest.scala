package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class OfferTest extends FreeSpec {

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

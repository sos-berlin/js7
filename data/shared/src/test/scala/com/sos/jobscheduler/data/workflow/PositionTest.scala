package com.sos.jobscheduler.data.workflow

import cats.syntax.option.catsSyntaxOptionId
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Position._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PositionTest extends FreeSpec {

  "test" in {
    Position(0)
    intercept[IllegalArgumentException] {
      Position(-1)
    }
  }

  "Fork" in {
    val a = Position(1) / "A" / 2
    assert(a == Position(1, "A", 2))
    assert(a == Position(Position.Parent(InstructionNr(1), OrderId.ChildId("A")) :: Nil, InstructionNr(2)))
    assert(a.dropChild == Position(1).some)
    assert((a / "B" / 3).dropChild == a.some)
    assert(Position(1).dropChild == None)
    assert(Position(1) / "A" / 2 / "B" / 3  ==
      Position(
        Position.Parent(InstructionNr(1), OrderId.ChildId("A")) ::
          Position.Parent(InstructionNr(2), OrderId.ChildId("B")) :: Nil,
        InstructionNr(3)))
  }

  "JSON" in {
    testJson(Position(7), """[ 7 ]""")
    testJson(Position(1) / "A" / 2, """[ 1, "A", 2 ]""")
  }
}

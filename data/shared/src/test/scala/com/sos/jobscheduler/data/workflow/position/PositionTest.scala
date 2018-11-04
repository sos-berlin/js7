package com.sos.jobscheduler.data.workflow.position

import cats.syntax.option.catsSyntaxOptionId
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PositionTest extends FreeSpec {

  "JSON" in {
    testJson(Position(1)               , json"""[ 1 ]""")
    testJson(Position(1) / "BRANCH"    , json"""[ 1, "BRANCH" ]""")
    testJson(Position(1) / "BRANCH" / 3, json"""[ 1, "BRANCH", 3 ]""")
    testJson(Position(1) / 2           , json"""[ 1, 2 ]""")
    testJson(Position(1) / 2 / 3       , json"""[ 1, 2, 3 ]""")
    testJson(Position(1) / 2 / 3 / 4   , json"""[ 1, 2, 3, 4 ]""")

    assert("""[ 1, 2 ]""".parseJson.as[Position].isLeft/*failed*/)
  }

  "Represented as array of simple types" in {
    assert(Position(1)                 .asSeq == Vector(1))
    assert((Position(1) / "BRANCH" / 3).asSeq == Vector(1, "BRANCH", 3))
    assert((Position(1) / 2 / 3)       .asSeq == Vector(1, 2, 3))
  }

  "Represented as array of JSON types" in {
    assert(Position(1)                 .asJsonArray == Vector(1.asJson))
    assert((Position(1) / "BRANCH" / 3).asJsonArray == Vector(1.asJson, "BRANCH".asJson, 3.asJson))
    assert((Position(1) / 2 / 3)       .asJsonArray == Vector(1.asJson, 2.asJson, 3.asJson))
  }

  "test" in {
    Position(0)
    intercept[IllegalArgumentException] {
      Position(-1)
    }
  }

  "BranchPath" in {
    assert((Nil / 7) == Position(7))
    //assert(BranchPath.NonEmpty(Position(1), BranchId(2)) / Position(3, 4, 5) ==
    //  Position(BranchPath.Segment(1, 2) :: BranchPath.Segment(3, 4) :: Nil, 5))
  }

  "Fork" in {
    val a = Position(1) / "A" / 2
    assert(a == Position(1, "A", 2))
    assert(a == Position(BranchPath.Segment(InstructionNr(1), "A") :: Nil, InstructionNr(2)))
    assert(a.dropChild == Position(1).some)
    assert((a / "B" / 3).dropChild == a.some)
    assert(Position(1).dropChild == None)
    assert(Position(1) / "A" / 2 / "B" / 3  ==
      Position(
        BranchPath.Segment(InstructionNr(1), "A") ::
          BranchPath.Segment(InstructionNr(2), "B") :: Nil,
        InstructionNr(3)))
  }
}

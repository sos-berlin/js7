package com.sos.jobscheduler.data.workflow

import cats.syntax.option.catsSyntaxOptionId
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.workflow.Position._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PositionTest extends FreeSpec {

  "JSON" in {
    testJson(Position(1)               , json"""[ 1 ]""")
    testJson(Position(1) / "BRANCH" / 2, json"""[ 1, "BRANCH", 2 ]""")
    testJson(Position(1) / 2 / 3       , json"""[ 1, 2, 3 ]""")

    assert("""[ 1, 2 ]""".parseJson.as[Position].isLeft/*failed*/)
  }

  "Represented as array of simple types" in {
    assert(Position(1)                 .asSeq == Vector(1))
    assert((Position(1) / "BRANCH" / 2).asSeq == Vector(1, "BRANCH", 2))
    assert((Position(1) / 2 / 2)       .asSeq == Vector(1, 2, 2))
  }

  "Represented as array of JSON types" in {
    assert(Position(1)                 .asJsonArray == Vector(1.asJson))
    assert((Position(1) / "BRANCH" / 2).asJsonArray == Vector(1.asJson, "BRANCH".asJson, 2.asJson))
    assert((Position(1) / 2 / 2)       .asJsonArray == Vector(1.asJson, 2.asJson, 2.asJson))
  }

  "test" in {
    Position(0)
    intercept[IllegalArgumentException] {
      Position(-1)
    }
  }

  "Parents" in {
    assert((Position.Parents.Empty / 7) == Position(7))
    assert((Position.Parents.NonEmpty(Position(1), Position.BranchId(2)) / 3) == Position(1, 2, 3))
  }

  "Fork" in {
    val a = Position(1) / "A" / 2
    assert(a == Position(1, "A", 2))
    assert(a == Position(Position.Parent(InstructionNr(1), "A") :: Nil, InstructionNr(2)))
    assert(a.dropChild == Position(1).some)
    assert((a / "B" / 3).dropChild == a.some)
    assert(Position(1).dropChild == None)
    assert(Position(1) / "A" / 2 / "B" / 3  ==
      Position(
        Position.Parent(InstructionNr(1), "A") ::
          Position.Parent(InstructionNr(2), "B") :: Nil,
        InstructionNr(3)))
  }

}

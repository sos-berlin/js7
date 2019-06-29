package com.sos.jobscheduler.data.workflow.position

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.option.catsSyntaxOptionId
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.workflow.position.BranchId.{Catch_, Then, Try_, catch_, try_}
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
    testJson(Position(1) / "BRANCH" % 3, json"""[ 1, "BRANCH", 3 ]""")
    testJson(Position(1) / 2           , json"""[ 1, 2 ]""")
    testJson(Position(1) / 2 % 3       , json"""[ 1, 2, 3 ]""")
    testJson(Position(1) / 2 % 3 / 4   , json"""[ 1, 2, 3, 4 ]""")

    assert("""[ 1, 2 ]""".parseJsonOrThrow.as[Position].isLeft/*failed*/)
  }

  "Represented as array of simple types" in {
    assert(Position(1)                 .asSeq == Vector(1))
    assert((Position(1) / "BRANCH" % 3).asSeq == Vector(1, "BRANCH", 3))
    assert((Position(1) / 2 % 3)       .asSeq == Vector(1, 2, 3))
  }

  "Represented as array of JSON types" in {
    assert(Position(1)                 .asJsonArray == Vector(1.asJson))
    assert((Position(1) / "BRANCH" % 3).asJsonArray == Vector(1.asJson, "BRANCH".asJson, 3.asJson))
    assert((Position(1) / 2 % 3)       .asJsonArray == Vector(1.asJson, 2.asJson, 3.asJson))
  }

  "test" in {
    Position(0)
    intercept[IllegalArgumentException] {
      Position(-1)
    }
  }

  "dropChild" in {
    assert(Position(1).dropChild == None)
    assert((Position(1) / 2 % 3).dropChild == Some(Position(1)))
    assert((Position(1) / 2 % 3 / 4 % 5).dropChild == Some(Position(1) / 2 % 3))
  }

  "splitBranchAndNr" in {
    assert(Position(1).splitBranchAndNr == None)
    assert((Position(1) / 2 % 3).splitBranchAndNr == Some((Position(1), BranchId(2), InstructionNr(3))))
    assert((Position(1) / 2 % 3 / 4 % 5).splitBranchAndNr == Some((Position(1) / 2 % 3, BranchId(4), InstructionNr(5))))
  }

  "BranchPath" in {
    assert((Nil % 7) == Position(7))
    //assert(BranchPath.NonEmpty(Position(1), BranchId(2)) / Position(3, 4, 5) ==
    //  Position(BranchPath.Segment(1, 2) :: BranchPath.Segment(3, 4) :: Nil, 5))
  }

  "toString" in {
    assert(Position(0).toString == "0")
    assert((Position(0) / "BRANCH" % 1).toString == "0/BRANCH:1")
  }

  "Fork" in {
    val a = Position(1) / "A" % 2
    assert(a == Position(1) / "A" % 2)
    assert(a == Position(BranchPath.Segment(InstructionNr(1), "A") :: Nil, InstructionNr(2)))
    assert(a.dropChild == Position(1).some)
    assert((a / "B" % 3).dropChild == a.some)
    assert(Position(1).dropChild == None)
    assert(Position(1) / "A" % 2 / "B" % 3  ==
      Position(
        BranchPath.Segment(InstructionNr(1), "A") ::
          BranchPath.Segment(InstructionNr(2), "B") :: Nil,
        InstructionNr(3)))
  }

  "forkPosition" in {
    assert(Position(1).forkPosition.isEmpty)
    assert((Position(1) / "fork+A" % 2).forkPosition == Some(Position(1)))
    assert((Position(1) / "fork+A" % 2 / Then % 3).forkPosition == Some(Position(1)))
    assert((Position(1) / "fork+A" % 2 / "fork+B" % 3).forkPosition == Some(Position(1) / "fork+A" % 2))
    assert((Position(1) / "fork+A" % 2 / "fork+B" % 3 / Then % 4).forkPosition == Some(Position(1) / "fork+A" % 2))
  }

  "isInFork" in {
    assert(!Position(1).isInFork)
    assert((Position(1) / "fork+A" % 2).isInFork)
    assert((Position(1) / "fork+A" % 2 / Then % 3).isInFork)
    assert((Position(1) / "fork+A" % 2 / "fork+B" % 3).isInFork)
    assert((Position(1) / "fork+A" % 2 / "fork+B" % 3 / Then % 4).isInFork)
  }

  "fork Branch" in {
    assert(Position(1).forkBranch == Nil)
    assert((Position(1) / "fork+A" % 2).forkBranch == Position(1) / "fork+A")
    assert((Position(1) / "fork+A" % 2 / Then % 3).forkBranch == Position(1) / "fork+A")
    assert((Position(1) / "fork+A" % 2 / "fork+B" % 3).forkBranch == Position(1) / "fork+A" % 2 / "fork+B")
    assert((Position(1) / "fork+A" % 2 / "fork+B" % 3 / Then % 4).forkBranch == Position(1) / "fork+A" % 2 / "fork+B")
  }

  "tryCount" in {
    assert(Position(0).tryCount == 0)    // Not in a try/catch
    assert(Position(99).tryCount == 0)   // No instruction
    assert((Position(1) / Try_ % 0).tryCount == 1)
    assert((Position(1) / try_(1) % 0).tryCount == 2)
    assert((Position(1) / try_(2) % 0).tryCount == 3)
    assert((Position(1) / Catch_ % 0).tryCount == 1)
    assert((Position(1) / catch_(1) % 0).tryCount == 2)
    assert((Position(1) / catch_(2) % 0).tryCount == 3)
  }

  "catchCount" in {
    assert(Position(0).tryCount == 0)    // Not in a try/catch
    assert(Position(99).tryCount == 0)   // No instruction
    assert((Position(1) / Try_ % 0).catchCount == 0)
    assert((Position(1) / try_(1) % 0).catchCount == 1)
    assert((Position(1) / try_(2) % 0).catchCount == 2)
    assert((Position(1) / Catch_ % 0).catchCount == 1)
    assert((Position(1) / catch_(1) % 0).catchCount == 2)
    assert((Position(1) / catch_(2) % 0).catchCount == 3)
  }

  "nextRetryBranchPath" in {
    assert(Position(0).nextRetryBranchPath == Invalid(Problem("Retry, but not in a catch-block")))
    assert(Position(99).nextRetryBranchPath == Invalid(Problem("Retry, but not in a catch-block")))
    assert((Position(1) / Try_ % 0).nextRetryBranchPath == Invalid(Problem("Retry, but not in a catch-block")))
    assert((Position(1) / try_(1) % 0).nextRetryBranchPath == Invalid(Problem("Retry, but not in a catch-block")))
    assert((Position(1) / Catch_ % 0).nextRetryBranchPath == Valid(Position(1) / try_(1)))
    assert((Position(1) / catch_(1) % 0).nextRetryBranchPath == Valid(Position(1) / try_(2)))
    assert((Position(1) / catch_(2) % 0).nextRetryBranchPath == Valid(Position(1) / try_(3)))
  }
}

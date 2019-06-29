package com.sos.jobscheduler.data.workflow.position

import cats.syntax.show._
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.workflow.position.BranchPath.Segment
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class BranchPathTest extends FreeSpec
{
  "JSON" in {
    testJson(Nil: BranchPath                      , json"""[]""")
    testJson(Segment(1, "BRANCH") :: Nil          , json"""[ 1, "BRANCH" ]""")
    testJson(Segment(1, 2) :: Nil                 , json"""[ 1, 2 ]""")
    testJson(Segment(1, 2) :: Segment(3, 4) :: Nil, json"""[ 1, 2, 3, 4 ]""")

    assert("""[ 1 ]""".parseJsonOrThrow.as[BranchPath].isLeft/*failed*/)
    assert("""[ 1, 2, 3 ]""".parseJsonOrThrow.as[BranchPath].isLeft/*failed*/)
  }

  "BranchPath and Position" in {
    assert(Nil % 1 == Position(1))
    assert((Segment(1, 2) :: Nil) % 3 == Position(1) / 2 % 3)
  }

  "PositionAndBranchId" in {
    intercept[MatchError] {
      Nil match {
        //case Nil =>
        case BranchPath.PositionAndBranchId(_, _) =>
      }
    }
    Segment(1, 2) :: Nil match {
      case BranchPath.PositionAndBranchId(Position(Nil, InstructionNr(1)), BranchId.Indexed(2)) =>
    }
    Segment(1, 2) :: Segment(3, 4) :: Nil match {
      case BranchPath.PositionAndBranchId(Position(Segment(InstructionNr(1), BranchId.Indexed(2)) :: Nil, InstructionNr(3)), BranchId.Indexed(4)) =>
    }
  }

  "Operator %" in {
    assert(Segment(1, 2) :: Nil == Position(1) / 2)
    assert(Segment(1, 2) :: Segment(3, 4) :: Nil == Position(1) / 2 % 3 / 4)
  }

  "dropChild" in {
    intercept[IllegalStateException] { Nil.dropChild }
    assert((Position(1) / 2).dropChild == Nil)
    assert((Position(1) / 2 % 3 / 4).dropChild == Position(1) / 2)
  }

  "parent" in {
    assert(Nil.parent.isEmpty)
    assert((Position(1) / 2).parent == Some(Position(1)))
    assert((Position(1) / 2 % 3 / 4).parent == Some(Position(1) / 2 % 3))
  }

  "show" in {
    assert(List.empty[Segment].show == "")
    assert((Segment(1, 2) :: Nil).show == "1/2")
    assert((Segment(1, 2) :: Segment(3, "BRANCH") :: Nil).show == "1/2:3/BRANCH")
  }

  "normalize" in {
    assert(BranchPath.normalize(Position(0) / "X") == (Position(0) / "X"))
    assert(BranchPath.normalize(Position(0) / 0) == (Position(0) / 0))
    assert(BranchPath.normalize(Position(0) / "try") == (Position(0) / "try"))
    assert(BranchPath.normalize(Position(0) / "try+0") == (Position(0) / "try"))
    assert(BranchPath.normalize(Position(0) / "try+1") == (Position(0) / "try"))
    assert(BranchPath.normalize(Position(0) / "try+123") == (Position(0) / "try"))
    assert(BranchPath.normalize(Position(0) / "catch") == (Position(0) / "catch"))
    assert(BranchPath.normalize(Position(0) / "catch+0") == (Position(0) / "catch"))
    assert(BranchPath.normalize(Position(0) / "catch+123") == (Position(0) / "catch"))
  }
}

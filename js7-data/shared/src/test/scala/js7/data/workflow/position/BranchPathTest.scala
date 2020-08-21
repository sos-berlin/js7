package js7.data.workflow.position

import cats.syntax.show._
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.data.workflow.position.BranchPath.Segment
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class BranchPathTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(Nil: BranchPath                          , json"""[]""")
    testJson(Segment(1, "BRANCH") :: Nil              , json"""[ 1, "BRANCH" ]""")
    testJson(Segment(1, "A") :: Segment(2, "B") :: Nil, json"""[ 1, "A", 2, "B" ]""")

    assert("""[ 1 ]""".parseJsonCheckedAs[BranchPath] == Left(Problem("JSON DecodingFailure at : Not a valid BranchPath")))
    assert("""[ 1, "BRANCH"]""".parseJsonCheckedAs[BranchPath] == Right(List(BranchPath.Segment(1, "BRANCH"))))
    assert("""[ 1, 2 ]""".parseJsonCheckedAs[BranchPath] == Left(Problem("JSON DecodingFailure at : String")))
    assert("""[ 1, 2, 3 ]""".parseJsonCheckedAs[BranchPath]== Left(Problem("JSON DecodingFailure at : Not a valid BranchPath")))
  }

  "BranchPath and Position" in {
    assert(Nil % 1 == Position(1))
    assert((Segment(1, "BRANCH") :: Nil) % 3 == Position(1) / "BRANCH" % 3)
  }

  "PositionAndBranchId" in {
    intercept[MatchError] {
      Nil match {
        //case Nil =>
        case BranchPath.PositionAndBranchId(_, _) =>
      }
    }
    Segment(1, "BRANCH") :: Nil match {
      case BranchPath.PositionAndBranchId(Position(Nil, InstructionNr(1)), BranchId.Named("BRANCH")) =>
    }
    Segment(1, "A") :: Segment(3, "B") :: Nil match {
      case BranchPath.PositionAndBranchId(
        Position(Segment(InstructionNr(1), BranchId.Named("A")) :: Nil, InstructionNr(3)),
        BranchId.Named("B")) =>
    }
  }

  "Operator %" in {
    assert(Segment(1, "BRANCH") :: Nil == Position(1) / "BRANCH")
    assert(Segment(1, "A") :: Segment(2, "B") :: Nil == Position(1) / "A" % 2 / "B")
  }

  "dropChild" in {
    intercept[IllegalStateException] { Nil.dropChild }
    assert((Position(1) / "A").dropChild == Nil)
    assert((Position(1) / "A" % 2 / "B").dropChild == Position(1) / "A")
  }

  "parent" in {
    assert(Nil.parent.isEmpty)
    assert((Position(1) / "A").parent == Some(Position(1)))
    assert((Position(1) / "A" % 2 / "B").parent == Some(Position(1) / "A" % 2))
  }

  "show" in {
    assert(List.empty[Segment].show == "")
    assert((Segment(1, "BRANCH") :: Nil).show == "1/BRANCH")
    assert((Segment(1, "A") :: Segment(2, "B") :: Nil).show == "1/A:2/B")
  }

  "normalize" in {
    assert(BranchPath.normalize(Position(0) / "X") == (Position(0) / "X"))
    assert(BranchPath.normalize(Position(0) / "try") == (Position(0) / "try"))
    assert(BranchPath.normalize(Position(0) / "try+0") == (Position(0) / "try"))
    assert(BranchPath.normalize(Position(0) / "try+1") == (Position(0) / "try"))
    assert(BranchPath.normalize(Position(0) / "try+123") == (Position(0) / "try"))
    assert(BranchPath.normalize(Position(0) / "catch") == (Position(0) / "catch"))
    assert(BranchPath.normalize(Position(0) / "catch+0") == (Position(0) / "catch"))
    assert(BranchPath.normalize(Position(0) / "catch+123") == (Position(0) / "catch"))
  }
}

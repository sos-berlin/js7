package js7.data.workflow.position

import js7.base.problem.Problem
import js7.base.test.Test
import js7.base.time.Timestamp
import js7.data.order.CycleState
import js7.data.workflow.position.BranchId.{Else, Then, catch_, fork, try_}

/**
  * @author Joacim Zschimmer
  */
final class BranchIdTest extends Test
{
  "then" in {
    assert(Then == BranchId.Named("then"))
  }

  "else" in {
    assert(Else == BranchId.Named("else"))
  }

  "try_" in {
    intercept[IllegalArgumentException](try_(-1))
    assert(try_(0) == BranchId.Named("try+0"))
    assert(try_(1) == BranchId.Named("try+1"))
  }

  "catch_" in {
    intercept[IllegalArgumentException](catch_(-1))
    assert(catch_(0) == BranchId.Named("catch+0"))
    assert(catch_(1) == BranchId.Named("catch+1"))
  }

  "fork" in {
    assert(fork("A") == BranchId.Named("fork+A"))
  }

  "cycle" - {
    "invalid" in {
      assert(BranchId("cycle+x").toCycleState == Left(Problem(
        "Expected a Cycle BranchId but got: cycle+x")))
      assert(BranchId("cycle+end=11122233344455566677").toCycleState == Left(Problem(
        "Expected a Cycle BranchId but got: cycle+end=11122233344455566677" +
          " - NumberFormatException: For input string: \"11122233344455566677\"")))
    }

    "scheme" in {
      checkCycle(
        CycleState(
          end = Timestamp("2021-09-30T00:00:00Z"),
          schemeIndex = 3,
          index = 7,
          next = Timestamp.Epoch),
        "cycle+end=1632960000000,scheme=3,i=7")
    }

    "index" in {
      checkCycle(
        CycleState(
          end = Timestamp("2021-09-30T00:00:00Z"),
          schemeIndex = 0,
          index = 7,
          next = Timestamp.Epoch),
        "cycle+end=1632960000000,i=7")
    }

    "next" in {
      checkCycle(
        CycleState(
          end = Timestamp("2021-09-30T00:00:00Z"),
          schemeIndex = 0,
          index = 7,
          next = Timestamp("2021-10-04T12:00:00Z")),
        "cycle+end=1632960000000,i=7,next=1633348800000")
    }

    "mixed" in {
      checkCycle(
        CycleState(
          end = Timestamp("2021-09-30T00:00:00Z"),
          schemeIndex = 3,
          index = 7,
          next = Timestamp("2021-10-04T12:00:00Z")),
        "cycle+end=1632960000000,scheme=3,i=7,next=1633348800000")
    }

    def checkCycle(cycleState: CycleState, branchIdString: String): Unit = {
      val branchId = BranchId.Named(branchIdString)
      assert(BranchId.cycle(cycleState) == branchId)
      assert(branchId.toCycleState == Right(cycleState))
    }
  }

  "IsFailureBoundary.unapply" in {
    BranchId("A") match {
      case BranchId.IsFailureBoundary(_) => fail()
      case _ =>
    }
    BranchId.fork("A") match {
      case BranchId.IsFailureBoundary(_) =>
      case _ => fail()
    }
    BranchId.ForkList match {
      case BranchId.IsFailureBoundary(BranchId.ForkList) =>
      case _ => fail()
    }
  }
}

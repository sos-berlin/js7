package js7.data.workflow.position

import js7.data.workflow.position.BranchId.{Else, Then, catch_, fork, try_}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class BranchIdTest extends AnyFreeSpec
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

package js7.data.workflow.position

import js7.data.workflow.instructions.Fork
import org.scalatest.freespec.AnyFreeSpec

final class ForkBranchIdTest extends AnyFreeSpec {
  "unapply" in {
    BranchId("A") match {
      case ForkBranchId(_) => fail()
      case _ =>
    }
    BranchId("fork+A") match {
      case ForkBranchId(Fork.Branch.Id("A")) =>
      case _ => fail()
    }
  }
}

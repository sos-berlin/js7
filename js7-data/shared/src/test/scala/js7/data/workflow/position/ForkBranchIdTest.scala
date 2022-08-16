package js7.data.workflow.position

import js7.base.test.Test
import js7.data.workflow.instructions.Fork

final class ForkBranchIdTest extends Test {
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

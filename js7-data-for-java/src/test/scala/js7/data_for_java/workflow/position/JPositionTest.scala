package js7.data_for_java.workflow.position

import js7.base.test.Test
import js7.data.workflow.position.{BranchId, Position}

final class JPositionTest extends Test
{
  "Java test" in {
    new JPositionTester(JPosition(Position(1) / BranchId.Then % 2 / BranchId.fork("BRANCH") % 3))
      .test()
  }
}

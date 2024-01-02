package js7.data_for_java.workflow.position

import js7.base.test.OurTestSuite
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position}

final class JPositionTest extends OurTestSuite:
  "Java test" in:
    new JPositionTester(JPosition(Position(1) / BranchId.Then % 2 / BranchId.fork("BRANCH") % 3))
      .test()

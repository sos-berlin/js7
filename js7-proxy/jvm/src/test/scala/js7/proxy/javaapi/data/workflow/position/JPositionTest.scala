package js7.proxy.javaapi.data.workflow.position

import js7.data.workflow.position.{BranchId, Position}
import org.scalatest.freespec.AnyFreeSpec

final class JPositionTest extends AnyFreeSpec
{
  "Java test" in {
    new JPositionTester(JPosition(Position(1) / BranchId.Then % 2 / BranchId.fork("BRANCH") % 3))
      .test()
  }
}

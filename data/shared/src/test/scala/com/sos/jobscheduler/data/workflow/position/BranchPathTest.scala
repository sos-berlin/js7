package com.sos.jobscheduler.data.workflow.position

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
    testJson(Nil: BranchPath                    , json"""[]""")
    testJson(Segment(1, "BRANCH") :: Nil         , json"""[ 1, "BRANCH" ]""")
    testJson(Segment(1, 2) :: Nil                , json"""[ 1, 2 ]""")
    testJson(Segment(1, 2) :: Segment(3, 4) :: Nil, json"""[ 1, 2, 3, 4 ]""")

    assert("""[ 1 ]""".parseJson.as[BranchPath].isLeft/*failed*/)
    assert("""[ 1, 2, 3 ]""".parseJson.as[BranchPath].isLeft/*failed*/)
  }

  "BranchPath and Position" in {
    assert(Nil / 1 == Position(1))
    assert((Segment(1, 2) :: Nil) / 3 == Position(1, 2, 3))
  }
}

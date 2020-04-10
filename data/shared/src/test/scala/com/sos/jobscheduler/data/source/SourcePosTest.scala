package com.sos.jobscheduler.data.source

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SourcePosTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(SourcePos(1, 2), json"""[1, 2]""")
  }
}

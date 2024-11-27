package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced.{DontWait, SkipWhenNoNotice, Wait}
import js7.tester.CirceJsonTester.testJson

final class ExpectOrConsumeNoticesInstructionTest extends OurTestSuite:

  "JSON" - {
    "WhenNotAnnounced" in:
      testJson[WhenNotAnnounced](Wait, json""" "Wait" """)
      testJson[WhenNotAnnounced](DontWait, json""" "DontWait" """)
      testJson[WhenNotAnnounced](SkipWhenNoNotice, json""" "SkipWhenNoNotice" """)
  }

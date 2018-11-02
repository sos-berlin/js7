package com.sos.jobscheduler.data.job

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.workflow.Position
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.tester.CirceJsonTester._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class LocalJobKeyTest extends FreeSpec
{
  "JSON" - {
    "LocalJobKey.Anonymous" in {
      testJson[LocalJobKey](LocalJobKey.Anonymous(Position(1, 2, 3)),
        json"""[ 1, 2, 3 ]""")
    }

    "LocalJobKey.Named" in {
      testJson[LocalJobKey](LocalJobKey.Named(WorkflowJob.Name("NAME")),
        json""" "NAME" """)
    }
  }
}

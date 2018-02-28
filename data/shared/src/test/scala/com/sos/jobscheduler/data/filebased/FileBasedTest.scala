package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedTest extends FreeSpec
{
  "Versioned JSON" in {
    testJson(
      FileBased.Versioned(
        FileBasedVersion("1.0"),
        TestFileBased(TestPath("/PATH"), "CONTENT")),
      json"""{
        "version": "1.0",
        "path": "/PATH",
        "content": "CONTENT"
      }""")
  }
}

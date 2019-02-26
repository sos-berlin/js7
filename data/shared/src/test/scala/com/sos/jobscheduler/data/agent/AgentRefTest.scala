package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentRefTest extends FreeSpec
{
  "JSON, anonymous" in {
    testJson(AgentRef(AgentRefPath.NoId, "http://127.0.0.1"),
      json"""{
        "uri": "http://127.0.0.1"
      }""")
  }

  "JSON, with ID" in {
    testJson(AgentRef(AgentRefPath("/AGENT") % "VERSION", "http://127.0.0.1"),
      json"""{
        "path": "/AGENT",
        "versionId": "VERSION",
        "uri": "http://127.0.0.1"
      }""")
  }
}

package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentTest extends FreeSpec
{
  "JSON, anonymous" in {
    testJson(Agent(AgentPath.NoId, "http://127.0.0.1"),
      json"""{
        "uri": "http://127.0.0.1"
      }""")
  }

  "JSON, with ID" in {
    testJson(Agent(AgentPath("/AGENT") % "VERSION", "http://127.0.0.1"),
      json"""{
        "path": "/AGENT",
        "versionId": "VERSION",
        "uri": "http://127.0.0.1"
      }""")
  }
}

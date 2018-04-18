package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentTest extends FreeSpec
{
  "JSON" in {
    testJson(Agent(AgentPath("/AGENT") % "VERSION", "http://127.0.0.1"),
      json"""{
        "id": {
          "path": "/AGENT",
          "versionId": "VERSION"
        },
        "uri": "http://127.0.0.1"
      }""")
  }
}

package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.agent.data.AgentTaskId
import org.scalatest.FreeSpec
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class CloseTaskTest extends FreeSpec {

  "JSON" in {
    val obj = CloseTask(AgentTaskId("111-222"), kill = true)
    val json = """{
      "$TYPE": "CloseTask",
      "agentTaskId": "111-222",
      "kill": true
    }""".parseJson
    assert((obj: Command).toJson == json)   // Command serializer includes $TYPE
    assert(obj == json.convertTo[Command])
  }
}

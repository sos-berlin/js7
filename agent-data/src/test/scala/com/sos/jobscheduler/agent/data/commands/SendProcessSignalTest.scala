package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.base.process.ProcessSignal.SIGTERM
import org.scalatest.FreeSpec
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class SendProcessSignalTest extends FreeSpec {

  "JSON" in {
    val obj = SendProcessSignal(AgentTaskId("111-222"), SIGTERM)
    val json = """{
      "$TYPE": "SendProcessSignal",
      "agentTaskId": "111-222",
      "signal": "SIGTERM"
    }""".parseJson
    assert((obj: Command).toJson == json)   // Command serializer includes $TYPE
    assert(obj == json.convertTo[Command])
  }
}

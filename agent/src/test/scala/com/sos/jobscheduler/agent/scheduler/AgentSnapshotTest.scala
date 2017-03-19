package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.common.auth.UserId
import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class AgentSnapshotTest extends FreeSpec {

  "JSON" in {
    check(AgentSnapshot.Master(UserId.Anonymous), """
      {
        "TYPE": "Master",
        "userId": "Anonymous"
      }""")
  }

  private def check(obj: Any, json: String): Unit = {
    val jsValue = json.parseJson
    assert(AgentSnapshot.jsonFormat.write(obj) == jsValue)
    assert(obj == AgentSnapshot.jsonFormat.read(jsValue))
  }
}

package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.agent.scheduler.AgentEvent.KeyedEventJsonFormat
import com.sos.jobscheduler.common.auth.UserId
import com.sos.jobscheduler.data.event.KeyedEvent
import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class AgentEventTest extends FreeSpec {

  "JSON" in {
    check(KeyedEvent(AgentEvent.MasterAdded)(UserId.Anonymous), """
      {
        "TYPE": "MasterAdded",
        "key": "Anonymous"
      }""")
  }

  private def check(keyedEvent: KeyedEvent[AgentEvent], json: String): Unit = {
    val jsValue = json.parseJson
    assert(keyedEvent.toJson == jsValue)
    assert(keyedEvent == jsValue.convertTo[KeyedEvent[AgentEvent]] )
  }
}

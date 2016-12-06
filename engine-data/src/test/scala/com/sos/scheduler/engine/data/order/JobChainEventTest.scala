package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, KeyedEvent}
import com.sos.scheduler.engine.data.events.SchedulerAnyKeyedEventJsonFormat
import com.sos.scheduler.engine.data.jobchain.{JobChainNodeAction, JobChainPath, JobChainState, NodeId}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class JobChainEventTest extends FreeSpec {

  "JobChainStateChanged" in {
    check(KeyedEvent(JobChainStateChanged(JobChainState.stopped))(JobChainPath("/JOBCHAIN")), """{
        "key": "/JOBCHAIN",
        "TYPE": "JobChainStateChanged",
        "state": "stopped"
      }""")
  }

  "JobChainNodeActionChanged" in {
    check(KeyedEvent(JobChainNodeActionChanged(NodeId("100"), JobChainNodeAction.next_state))(JobChainPath("/JOBCHAIN")), """{
        "key": "/JOBCHAIN",
        "TYPE": "JobChainNodeActionChanged",
        "nodeId": "100",
        "action": "next_state"
      }""")
  }

  private def check(event: AnyKeyedEvent, json: String): Unit = {
    val jsValue = json.parseJson
    assert(event.toJson == jsValue)
    assert(event == jsValue.convertTo[AnyKeyedEvent] )
  }
}

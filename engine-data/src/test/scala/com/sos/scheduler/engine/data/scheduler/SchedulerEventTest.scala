package com.sos.scheduler.engine.data.scheduler

import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, KeyedEvent}
import com.sos.scheduler.engine.data.events.SchedulerAnyKeyedEventJsonFormat
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SchedulerEventTest extends FreeSpec {

  "SchedulerStateChanged" in {
    check(KeyedEvent(SchedulerStateChanged(SchedulerState.running)), """{
        "TYPE": "SchedulerStateChanged",
        "state": "running"
      }""")
  }

  "SchedulerInitiated" in {
    check(KeyedEvent(SchedulerInitiated), """{
        "TYPE": "SchedulerInitiated"
      }""")
  }

  "SchedulerClosed" in {
    check(KeyedEvent(SchedulerClosed), """{
        "TYPE": "SchedulerClosed"
      }""")
  }

  private def check(event: AnyKeyedEvent, json: String): Unit = {
    val jsValue = json.parseJson
    assert(event.toJson == jsValue)
    assert(event == jsValue.convertTo[AnyKeyedEvent] )
  }
}

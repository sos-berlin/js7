package com.sos.scheduler.engine.data.job

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
final class JobEventTest extends FreeSpec {

  private val jobPath = JobPath("/JOB")

  "JobStateChanged" in {
    check(KeyedEvent(JobStateChanged(JobState.stopped))(jobPath),
      """{
        "key": "/JOB",
        "TYPE": "JobStateChanged",
        "state": "stopped"
      }""")
  }
  private def check(event: AnyKeyedEvent, json: String): Unit = {
    val jsValue = json.parseJson
    assert(event.toJson == jsValue)
    assert(event == jsValue.convertTo[AnyKeyedEvent] )
  }
}

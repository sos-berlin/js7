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
final class TaskEventTest extends FreeSpec {

  private val taskKey = TaskKey(JobPath("/JOB"), TaskId(4711))

  "TaskStarted" in {
    check(KeyedEvent(TaskStarted)(taskKey),
      """{
        "key": {
          "jobPath": "/JOB",
          "taskId": "4711"
        },
        "TYPE": "TaskStarted"
      }""")
  }

  "TaskEnded" in {
    check(KeyedEvent(TaskEnded(ReturnCode(1)))(taskKey),
      """{
        "key": {
          "jobPath": "/JOB",
          "taskId": "4711"
        },
        "TYPE": "TaskEnded",
        "returnCode": 1
      }""")
  }

  "TaskClosed" in {
    check(KeyedEvent(TaskClosed)(taskKey),
      """{
        "key": {
          "jobPath": "/JOB",
          "taskId": "4711"
        },
        "TYPE": "TaskClosed"
      }""")
  }

  private def check(event: AnyKeyedEvent, json: String): Unit = {
    val jsValue = json.parseJson
    assert(event.toJson == jsValue)
    assert(event == jsValue.convertTo[AnyKeyedEvent] )
  }
}

package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.test.JsonTest
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
final class TaskClosedEventJsonTest extends FunSuite with JsonTest {

  addJsonTests(
    new TaskClosedEvent(TaskId(123), JobPath("/PATH/JOB")),
    """{
      "TYPE": "TaskClosedEvent",
      "taskId": 123,
      "jobPath": "/PATH/JOB"
    }""")
}

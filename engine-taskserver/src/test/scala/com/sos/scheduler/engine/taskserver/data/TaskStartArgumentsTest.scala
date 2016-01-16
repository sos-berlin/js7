package com.sos.scheduler.engine.taskserver.data

import com.sos.scheduler.engine.agent.data.commands.StartTask
import com.sos.scheduler.engine.data.job.{JobPath, TaskId}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class TaskStartArgumentsTest extends FreeSpec {

  "logFilenamePart" in {
    assertResult("task-1-1-test-123") {
      TaskStartArguments.forTest().copy(startMeta = StartTask.Meta(JobPath("/folder/test"), TaskId(123))).logFilenamePart
    }
  }

  "logFilenamePart when master < v1.10.4" in {
    assertResult("task-1-1-(OLD-MASTER)--1") {
      TaskStartArguments.forTest().logFilenamePart
    }
  }
}

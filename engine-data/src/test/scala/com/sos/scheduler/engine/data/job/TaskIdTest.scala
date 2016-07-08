package com.sos.scheduler.engine.data.job

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class TaskIdTest extends FreeSpec {

  "TaskId.+-" in {
    assert(TaskId(10) + 1 == TaskId(11))
    assert(TaskId(10) - 1 == TaskId(9))
  }
}

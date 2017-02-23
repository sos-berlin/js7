package com.sos.scheduler.engine.data.job

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TaskIdTest extends FreeSpec {

  "TaskId.+-" in {
    assert(TaskId(10) + 1 == TaskId(11))
    assert(TaskId(10) - 1 == TaskId(9))
  }
}

package js7.data.job

import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TaskIdTest extends AnyFreeSpec {

  "TaskId.+-" in {
    assert(TaskId(10) + 1 == TaskId(11))
    assert(TaskId(10) - 1 == TaskId(9))
  }
}

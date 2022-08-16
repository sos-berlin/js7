package js7.data.job

import js7.base.test.Test
import org.scalatest.matchers.should.Matchers.*

/**
 * @author Joacim Zschimmer
 */
final class TaskIdTest extends Test
{
  "toString" in {
    assert(TaskId(123, 789).toString == "TaskId(123-789)")
    assert(TaskId(-123, 789).toString == "TaskId(-123-789)")
  }

  "apply(string)" in {
    assert(TaskId("123-789").string == "123-789")
    assert(TaskId("-123-789").string == "-123-789")
    assert(TaskId("123000000789").string == "123000000789")
    TaskId("allowed-characters-ABCDEFGHIJKLMNOPQRSTUVWXYZ-abcdefghijklmnopqrstuvwxyz-012356789_.")
    intercept[IllegalArgumentException] { TaskId("") }
    intercept[IllegalArgumentException] { TaskId("with space") }
    intercept[IllegalArgumentException] { TaskId("with-$variable") }
  }

  "TaskIdGenerator" in {
    val generator = TaskId.newGenerator()
    for (i <- 1 to 10000) {
      val id = generator.next()
      assert(id.index == i)
    }
  }

  "TaskIdGenerator overflow" in {
    val ids = (TaskId.newGenerator(start = Int.MaxValue - 2) take 5).toList
    ids.map(_.index) shouldEqual List(Int.MaxValue - 2, Int.MaxValue - 1, Int.MaxValue, 1, 2)
  }
}

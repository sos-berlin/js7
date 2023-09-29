package js7.base.utils

import js7.base.data.ByteArray
import monix.execution.Scheduler.Implicits.traced
import js7.base.test.OurAsyncTestSuite

final class ByteSequenceToLinesObservableTest extends OurAsyncTestSuite
{
  "empty" in {
    val obs = new ByteSequenceToLinesObservable[ByteArray]
    for list <- obs(ByteArray.empty).toListL.runToFuture yield
      assert(list.isEmpty)
  }

  "one line" in {
    val obs = new ByteSequenceToLinesObservable[ByteArray]
    for list <- obs(ByteArray("line\n")).toListL.runToFuture yield
      assert(list == List(ByteArray("line\n")))
  }

  "more lines" in {
    val obs = new ByteSequenceToLinesObservable[ByteArray]
    for list <- obs(ByteArray("line1\nline2\nline3\n")).toListL.runToFuture yield
      assert(list == List(ByteArray("line1\n"), ByteArray("line2\n"), ByteArray("line3\n")))
  }

  "split lines" - {
    lazy val obs = new ByteSequenceToLinesObservable[ByteArray]

    "part 1" in {
      for list <- obs(ByteArray("line-begin")).toListL.runToFuture yield
        assert(list == Nil)
    }

    "part 2" in {
      for list <- obs(ByteArray("...middle")).toListL.runToFuture yield
        assert(list == Nil)
    }

    "part 3" in {
      for list <- obs(ByteArray("...end\nline2")).toListL.runToFuture yield
        assert(list == List(ByteArray("line-begin...middle...end\n")))
    }

    "line2, line3" in {
      for list <- obs(ByteArray("\nline3\n")).toListL.runToFuture yield
        assert(list == List(ByteArray("line2\n"), ByteArray("line3\n")))
    }

    "line4" in {
      for list <- obs(ByteArray("line4\n")).toListL.runToFuture yield
        assert(list == List(ByteArray("line4\n")))
    }
  }
}

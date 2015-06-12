package com.sos.scheduler.engine.common.async

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Futures.awaitResult
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import java.time.Instant
import java.time.Instant.now
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable
import scala.concurrent.Promise

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ConcurrentCallerTest extends FreeSpec {

  "ConcurrentCaller" in {
    val instants = mutable.Buffer[Instant]()
    autoClosing(new ConcurrentCaller(List(10.ms, 30.ms, 100.ms, 3600.s), { () ⇒ instants += now() }, "TEST")) { backgroundCaller ⇒
      backgroundCaller.start()
      waitForCondition(timeout = 10.s, step = 10.ms) { instants.size == 4 }
    }
    val expectedSummedDurations = List(0.ms, 10.ms, 40.ms, 140.ms)
    val durations = instants map { _ - instants.head }
    for ((duration, expectedMinimum) ← durations zip expectedSummedDurations) assert(duration >= expectedMinimum)
  }

  "With limited pauses" in {
    var n = 0
    val pauses = List(0.ms, 0.ms)
    autoClosing(new ConcurrentCaller(pauses, () ⇒ n += 1, "TEST")) { backgroundCaller ⇒
      backgroundCaller.start()
      waitForCondition(timeout = 10.s, step = 10.ms) { n == pauses.size + 1 }
      sleep(50.ms)
      assert(n == pauses.size + 1)
    }
    assert(n == pauses.size + 1)
  }

  "Calls once with no period" in {
    var n = 0
    autoClosing(new ConcurrentCaller(Nil, () ⇒ n += 1, "TEST")) { backgroundCaller ⇒
      backgroundCaller.start()
      sleep(500.ms)
    }
    assert(n == 1)
  }

  "Terminates with exception" in {
    class TestException extends Exception
    autoClosing(new ConcurrentCaller(Nil, () ⇒ throw new TestException, "TEST")) { backgroundCaller ⇒
      backgroundCaller.start()
      intercept[TestException] { awaitResult(backgroundCaller.terminated, 10.s) }
    }
  }

  "close terminates waiting" in {
    val promise = Promise[Unit]()
    val backgroundCaller = new ConcurrentCaller(List(60.s), () ⇒ promise.success(()), "TEST")
      backgroundCaller.start()
      awaitResult(promise.future, 10.s)
      sleep(100.ms)
      backgroundCaller.close()
      awaitResult(backgroundCaller.terminated, 10.s)
  }
}

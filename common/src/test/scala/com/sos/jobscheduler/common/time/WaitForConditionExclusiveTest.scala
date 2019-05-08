package com.sos.jobscheduler.common.time

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition._
import com.sos.jobscheduler.common.time.WaitForConditionExclusiveTest._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.concurrent.duration._

final class WaitForConditionExclusiveTest extends FreeSpec {

  "Warm-up" in {
    realTimeIterator(Seq(Deadline.now)) // Aufruf zum Warmwerden. Laden der Klasse kann eine Weile dauern
    meterElapsedTime { retryUntil(99.s, 1.s) { 7 } }
    intercept[IllegalStateException] { throw new IllegalStateException }
  }

  "retryUntil" - {
    class Fun(n: Int) extends (() => Int) {
      var count = 0
      def apply() = {
        count += 1
        if (count >= n) 7 else throw new IllegalStateException("FAILED")
      }
    }

    "Immediate success" in {
      val fun = new Fun(1)
      meterElapsedTime {
        retryUntil(99.s, 10.s) { fun() }
      } should (be < 1.s)
      assert(fun.count == 1)
    }

    "Late success" in {
      val fun = new Fun(10)
      meterElapsedTime {
        retryUntil(99.s, 10.ms) { fun() }
      } should (be < 1.s)
      assert(fun.count == 10)
    }

    "Failure" in {
      val fun = new Fun(1000)
      meterElapsedTime {
        intercept[IllegalStateException] {
          retryUntil(100.ms, 10.ms) { fun() }
        }
      } should (be >= 100.ms)
      assert(fun.count > 1)
    }
  }

  "realTimeIterator (time-sensitive test)" in {
    meterElapsedTime { realTimeIterator(Seq(Deadline.now + 10.s)) } should be < 300.ms // Bereitstellung soll nicht warten
    val t0 = Deadline.now
    val (t1, t2, t3) = (t0 + 500.ms, t0 + 1500.ms, t0 + 2000.ms)
    val i = realTimeIterator(Seq(t1, t2, t3))
    meterElapsedTime { i.next() } .toMillis should be ((t1 - t0).toMillis +- 400)
    meterElapsedTime { i.next() } .toMillis should be ((t2 - t1).toMillis +- 400)
    meterElapsedTime { i.next() } .toMillis should be ((t3 - t2).toMillis +- 400)
  }

  "waitForCondition(TimeoutWithSteps) 0 steps (time-sensitive test)" in {
    val elapsed = meterElapsedTime { waitForCondition(TimeoutWithSteps(2.s, 1.s)) { true } }
    elapsed should be < 500.ms
  }

  "waitForCondition(TimeoutWithSteps) all steps (time-sensitive test)" in {
    val elapsed = meterElapsedTime { waitForCondition(TimeoutWithSteps(300.ms, 1.ms)) { false } }
    elapsed should (be >= 300.ms and be <= 700.ms)
  }

  "waitForCondition(TimeoutWithSteps) some steps (time-sensitive test)" in {
    var cnt = 0
    val elapsed = meterElapsedTime { waitForCondition(TimeoutWithSteps(1.s, 10.ms)) { cnt += 1; cnt > 10 } }  // 100ms
    elapsed should (be >= 100.ms and be < 400.ms)
  }
}

private object WaitForConditionExclusiveTest
{
  def meterElapsedTime(f: => Unit): FiniteDuration = {
    val start = System.nanoTime
    f
    (System.nanoTime - start).nanoseconds
  }
}

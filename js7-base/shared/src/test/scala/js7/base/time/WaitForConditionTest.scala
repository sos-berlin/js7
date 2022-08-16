package js7.base.time

import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.*
import js7.base.time.WaitForConditionTest.*
import monix.execution.schedulers.TestScheduler
import org.scalatest.matchers.should.Matchers.*
import scala.concurrent.duration.*

final class WaitForConditionTest extends OurTestSuite
{
  "retryUntil" - {
    class Fun(n: Int) {
      var count = 0
      def apply(): Unit = {
        count += 1
        if (count <= n) throw new IllegalStateException("FAILED")
      }
    }

    "Immediate success" in {
      implicit val sleeper = new TestSleeper
      val fun = new Fun(0)
      assert(meterElapsedTime {
        retryUntil(99.s, 10.s) { fun.apply() }
      } == 0.s)
      assert(fun.count == 1)
    }

    "Late success" in {
      implicit val sleeper = new TestSleeper
      val fun = new Fun(10)
      assert(meterElapsedTime {
        retryUntil(99.s, 7.ms) { fun.apply() }
      } == 10 * 7.ms)
      assert(fun.count == 11)
    }

    "Failure" in {
      implicit val sleeper = new TestSleeper
      val fun = new Fun(1000)
      assert(meterElapsedTime {
        intercept[IllegalStateException] {
          retryUntil(100.ms, 10.ms) { fun.apply() }
        }
      } == 100.ms)
      assert(fun.count > 1)
    }
  }

  "realTimeIterator (time-sensitive test)" in {
    implicit val sleeper = new TestSleeper
    meterElapsedTime { realTimeIterator(Seq(sleeper.now + 10.s)) } shouldEqual 0.s // Bereitstellung soll nicht warten
    val t0 = sleeper.now
    val (t1, t2, t3) = (t0 + 500.ms, t0 + 1500.ms, t0 + 2000.ms)
    val i = realTimeIterator(Seq(t1, t2, t3))
    meterElapsedTime { i.next() } shouldEqual t1 - t0
    meterElapsedTime { i.next() } shouldEqual t2 - t1
    meterElapsedTime { i.next() } shouldEqual t3 - t2
  }

  "waitForCondition 0 steps (time-sensitive test)" in {
    implicit val sleeper = new TestSleeper
    val elapsed = meterElapsedTime { waitForCondition(20.s, 10.s) { true } }
    elapsed should be < 1.s
  }

  "waitForCondition all steps (time-sensitive test)" in {
    implicit val sleeper = new TestSleeper
    val elapsed = meterElapsedTime { waitForCondition(300.ms, 1.ms) { false } }
    elapsed should (be >= 300.ms and be <= 3.s)
  }

  "waitForCondition some steps (time-sensitive test)" in {
    implicit val sleeper = new TestSleeper
    var cnt = 0
    val elapsed = meterElapsedTime { waitForCondition(1.s, 10.ms) { cnt += 1; cnt > 10 } }  // 100ms
    elapsed should (be >= 100.ms and be < 3.s)
  }
}

private object WaitForConditionTest
{
  def meterElapsedTime(f: => Unit)(implicit sleeper: Sleeper): FiniteDuration = {
    val since = sleeper.now
    f
    since.elapsed
  }

  private class TestSleeper extends Sleeper
  {
    private val scheduler = TestScheduler()

    def now = scheduler.now

    def sleep(duration: FiniteDuration) =
      scheduler.tick(duration)
  }
}

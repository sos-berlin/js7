package js7.base.time

import cats.effect.unsafe.IORuntime
import js7.base.monixlike.MonixLikeExtensions.scheduleAtFixedRate
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.tester.ScalaTestUtils.awaitAndAssert
import scala.collection.mutable

final class TestSchedulerTest extends OurAsyncTestSuite:

  private val start = ts"2026-10-05T00:00:00Z"
  private given IORuntime = ioRuntime

  "tick runs tasks in the order of their scheduled time, each at its time" in:
    val scheduler = TestScheduler(start, ioRuntime)
    val runs = mutable.Buffer.empty[(Int, Timestamp)]
    for i <- Seq(3, 1, 2) do
      scheduler.sleep(i.s, () => runs.synchronized(runs += i -> scheduler.now()))
    scheduler.tick(10.s)
    assert(runs == Seq(
      1 -> (start + 1.s),
      2 -> (start + 2.s),
      3 -> (start + 3.s)))
    assert(scheduler.now() == start + 10.s)

  "scheduleAtFixedRate" in:
    val scheduler = TestScheduler(start, ioRuntime)
    val x = Atomic(0)
    val cancel = scheduler.scheduleAtFixedRate(1.s, 1.s):
      x += 1
    scheduler.tick(10.s)
    assert(x.get() == 10)
    cancel.cancel()
    succeed

  "tick runs a task scheduled due while ticking" in:
    val scheduler = TestScheduler(start, ioRuntime)
    val x = Atomic(0)
    scheduler.sleep(1.s, () => scheduler.sleep(0.s, () => x += 1))
    scheduler.tick(1.s)
    assert(x.get() == 1)

  "sleep runs a due task immediately" in:
    val scheduler = TestScheduler(start, ioRuntime)
    val x = Atomic(0)
    scheduler.sleep(0.s,
      () => x += 1 // immediately started in background!
    )
    awaitAndAssert(x.get() == 1)

  "tick runs all due tasks, even if one throws" in:
    val scheduler = TestScheduler(start, ioRuntime)
    val x = Atomic(0)
    scheduler.sleep(1.s, () => throw IllegalStateException("TEST"))
    scheduler.sleep(1.s, () => x += 1)
    scheduler.sleep(2.s, () => x += 1)
    intercept[IllegalStateException]:
      scheduler.tick(2.s)
    assert(x.get() == 2 && scheduler.now() == start + 2.s)

  "tick with negative duration is rejected" in:
    val scheduler = TestScheduler(start, ioRuntime)
    intercept[IllegalArgumentException]:
      scheduler.tick((-1).s)
    assert(scheduler.now() == start)

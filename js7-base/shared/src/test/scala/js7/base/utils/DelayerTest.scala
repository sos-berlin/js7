package js7.base.utils

import cats.data.NonEmptySeq
import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import scala.collection.mutable
import scala.concurrent.duration.*

final class DelayerTest extends OurAsyncTestSuite
{
  private val conf = DelayConf(NonEmptySeq.of(1.s, 2.s, 3.s), resetWhen = 100.s)

  "sleep" in {
    implicit val scheduler = TestScheduler()
    val times = mutable.Buffer[FiniteDuration]()
    val start = scheduler.now
    val test =
      for
        delay <- Delayer.start[Task](conf)
        _ <- delay.sleep                  // +1s
        _ <- Task(times += start.elapsed) //    = 1s
        _ <- delay.sleep                  // +2s
        _ <- Task(times += start.elapsed) //    = 3s
        _ <- delay.sleep                  // +3s
        _ <- Task(times += start.elapsed) //    = 6s
        _ <- Task.sleep(1.s)              // 7s
        _ <- delay.sleep                  // +3s - 1s
        _ <- Task(times += start.elapsed) //    = 9s
        _ <- Task.sleep(4.s)              // 13s
        _ <- delay.sleep                  // +0s
        _ <- Task(times += start.elapsed) //    = 13s
        _ <- delay.sleep                  // +3s
        _ <- Task(times += start.elapsed) //    = 16s
        _ <- delay.sleep                  // +3s
        _ <- Task(times += start.elapsed) //    = 19s
        _ <- Task.sleep(100.s)            // +100s   resetWhen reached
        _ <- delay.sleep                  // 119s
        _ <- Task(times += start.elapsed) //    = 119s
        _ <- delay.sleep                  // +1s
        _ <- Task(times += start.elapsed) //    = 120s
        _ <- delay.sleep                  // +2s
        _ <- Task(times += start.elapsed) //    = 122s

      yield
        assert(times.map(_.toCoarsest) == mutable.Buffer(
          1.s, 3.s, 6.s, 9.s, 13.s, 16.s, 19.s, 119.s, 120.s, 122.s))

    val future = test.runToFuture
    scheduler.tick(1.h)
    future
  }

  "observable" in {
    implicit val scheduler = TestScheduler()
    val start = scheduler.now
    val test = Delayer
      .observable[Task](conf)
      .map(_ => start.elapsed)
      .take(6)
      .toListL
      .map(times =>
        assert(times.map(_.toCoarsest) == mutable.Buffer(
          0.s, 1.s, 3.s, 6.s, 9.s, 12.s)))

    val future = test.runToFuture
    scheduler.tick(1.h)
    future
  }
}

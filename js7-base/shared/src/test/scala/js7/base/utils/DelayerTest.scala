package js7.base.utils

import cats.data.NonEmptySeq
import cats.effect.IO
import cats.effect.testkit.TestControl
import js7.base.catsutils.CatsDeadline
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import scala.collection.mutable
import scala.concurrent.duration.*

final class DelayerTest extends OurAsyncTestSuite:
  private val conf = DelayConf(NonEmptySeq.of(1.s, 2.s, 3.s), resetWhen = 100.s)

  "sleep" in:
    TestControl.executeEmbed:
      val times = mutable.Buffer[FiniteDuration]()
      for
        start <- CatsDeadline.now
        delay <- Delayer.start[IO](conf)
        _ <- delay.sleep      // +1s
        t <- start.elapsed    //    = 1s
        _ <- IO(times += t)
        _ <- delay.sleep      // +2s
        t <- start.elapsed    //    = 3s
        _ <- IO(times += t)
        _ <- delay.sleep      // +3s
        t <- start.elapsed    //    = 6s
        _ <- IO(times += t)
        _ <- IO.sleep(1.s)    // 7s
        _ <- delay.sleep      // +3s - 1s
        t <- start.elapsed    //    = 9s
        _ <- IO(times += t)
        _ <- IO.sleep(4.s)    // 13s
        _ <- delay.sleep      // +0s
        t <- start.elapsed    //    = 13s
        _ <- IO(times += t)
        _ <- delay.sleep      // +3s
        t <- start.elapsed    //    = 16s
        _ <- IO(times += t)
        _ <- delay.sleep      // +3s
        t <- start.elapsed    //    = 19s
        _ <- IO(times += t)
        _ <- IO.sleep(100.s)  // +100s   resetWhen reached
        _ <- delay.sleep      // 119s
        t <- start.elapsed    //    = 119s
        _ <- IO(times += t)
        _ <- delay.sleep      // +1s
        t <- start.elapsed    //    = 120s
        _ <- IO(times += t)
        _ <- delay.sleep      // +2s
        t <- start.elapsed    //    = 122s
        _ <- IO(times += t)
      yield
        assert(times.map(_.toCoarsest) == mutable.Buffer(
          1.s, 3.s, 6.s, 9.s, 13.s, 16.s, 19.s, 119.s, 120.s, 122.s))

  "stream" in:
    TestControl.executeEmbed:
      CatsDeadline.now.flatMap(start =>
        Delayer
          .stream[IO](conf)
          .evalMap(_ => start.elapsed)
          .take(6)
          .compile
          .toVector
          .map(times =>
            assert(times.map(_.toCoarsest) == Vector(0.s, 1.s, 3.s, 6.s, 9.s, 12.s))))

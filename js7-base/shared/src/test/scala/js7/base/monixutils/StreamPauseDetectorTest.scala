package js7.base.monixutils

import js7.base.monixutils.StreamPauseDetector.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import cats.effect.IO
import monix.execution.schedulers.TestScheduler
import fs2.Stream
import scala.util.Success

final class StreamPauseDetectorTest extends OurTestSuite:

  private val pausingStream = Stream.fromIterable(1 to 5)
    .doOnNext(i => IO.sleep(
      if i == 3 then 2.s else if i == 4 then 3.s else 100.ms)) // Long pause before the third element

  "detectPauses" in:
    implicit val scheduler = TestScheduler()
    val future = pausingStream.detectPauses2(1.s).toListL.runToFuture
    scheduler.tick(100.s)
    assert(future.value == Some(Success(List(
      Right(1),
      Right(2),
      Left(MonixDeadline.fromNanos(1_000_000_000L)),
      Right(3),
      Left(MonixDeadline.fromNanos(3_000_000_000L)),
      Left(MonixDeadline.fromNanos(3_000_000_000L)),
      Right(4),
      Right(5)))))

  "detectPauses detects initial pause" in:
    implicit val scheduler = TestScheduler()
    val future = (Stream.fromIO(IO(0).delayBy(2.s)) ++ pausingStream)
      .detectPauses2(1001.ms).toListL.runToFuture
    scheduler.tick(100.s)
    assert(future.value == Some(Success(List(
      Left(MonixDeadline.fromNanos(0L)),
      Right(0),
      Right(1),
      Right(2),
      Left(MonixDeadline.fromNanos(3_003_000_000L)),
      Right(3),
      Left(MonixDeadline.fromNanos(5_005_000_000L)),
      Left(MonixDeadline.fromNanos(5_005_000_000L)),
      Right(4),
      Right(5)))))

  "echoRepeated"  in:
    implicit val scheduler = TestScheduler()
    val future = pausingStream.echoRepeated(800.ms).toListL.runToFuture
    scheduler.tick(100.s)
    assert(future.value == Some(Success(List(
      1,
      2, 2, 2,
      3, 3, 3, 3,
      4,
      5))))

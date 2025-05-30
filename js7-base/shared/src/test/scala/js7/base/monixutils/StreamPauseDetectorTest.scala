package js7.base.monixutils

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.syntax.flatMap.*
import fs2.Stream
import js7.base.catsutils.SyncDeadline
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.headL
import js7.base.monixutils.StreamPauseDetector.*
import js7.base.monixutils.StreamPauseDetectorTest.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import scala.concurrent.duration.Deadline

final class StreamPauseDetectorTest extends OurAsyncTestSuite:

  private val pausingStream: Stream[IO, Int] =
    Stream.iterable(1 to 5).covary[IO].flatTap(i => Stream.sleep(i match
      case 3 => 2.s // Long pause before the third element
      case 4 => 3.s // Long pause before the forth element
      case _ => 100.ms))

  "detectPauses" in :
    TestControl.executeEmbed:
      for
        result <- pausingStream.detectPauses_(1.s).compile.toList
      yield
        assert(result == List(
          Value(1),
          Value(2),
          Expired(SyncDeadline.fromMonotonic(1.s)),
          Value(3),
          Expired(SyncDeadline.fromMonotonic(3.s)),
          Expired(SyncDeadline.fromMonotonic(3.s)),
          Value(4),
          Value(5)))

  "detectPauses detects initial pause" in:
    TestControl.executeEmbed:
      for
        result <- Stream.sleep_[IO](2.s).append(pausingStream).detectPauses_(1001.ms)
          .compile.toList
      yield
        assert(result == List(
          Expired(SyncDeadline.fromMonotonic(0.s)),
          Expired(SyncDeadline.fromMonotonic(0.s)),
          Value(1),
          Value(2),
          Expired(SyncDeadline.fromMonotonic(3003.ms)),
          Value(3),
          Expired(SyncDeadline.fromMonotonic(5005.ms)),
          Expired(SyncDeadline.fromMonotonic(5005.ms)),
          Value(4),
          Value(5)))


  "Speed" in :
    if !sys.props.contains("test.speed") then
      IO.pure(succeed)
    else
      val n = 100_000

      IO.defer:
        val t = Deadline.now
        Stream.iterable(1 to n)
          .flatMap(Stream.emit) // Make single-element chunks
          .covary[IO]
          .fold(0L)(_ + _)
          .headL
          .map: sum =>
            logger.info(itemsPerSecondString(t.elapsed, n, "noop"))
            assert(sum == (n + 1L) * (n / 2))
      .productR:
        IO.defer:
          val t = Deadline.now
          Stream.iterable(1 to n)
            .flatMap(Stream.emit) // Make single-element chunks
            .detectPauses(1.s, null)
            .takeWhile(_ != null)
            .collect:
              case i: Int => i
            .fold(0L)(_ + _)
            .headL
            .map: sum =>
              // ~30000 chunks/s on MacBook Pro M1
              logger.info(itemsPerSecondString(t.elapsed, n, "detectPauses"))
              assert(sum == (n + 1L) * (n / 2))

object StreamPauseDetectorTest:
  private val logger = Logger[this.type]

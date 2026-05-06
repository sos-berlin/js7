package js7.base.monixutils

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.syntax.flatMap.*
import fs2.Stream
import js7.base.catsutils.SyncDeadline
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.headL
import js7.base.monixutils.PauseDetector.*
import js7.base.monixutils.PauseDetectorTest.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import scala.concurrent.duration.Deadline

final class PauseDetectorTest extends OurAsyncTestSuite:

  private val pausingStream: Stream[IO, Int] =
    Stream.iterable(1 to 5).covary[IO].flatTap: i =>
      Stream.sleep:
        i match
          case 3 => 2.s // Long pause before the third element
          case 4 => 3.s // Long pause before the fourth element
          case 5 => 900.ms // Late abot late heartbeat
          case _ => 100.ms

  "detectPauses" in :
    TestControl.executeEmbed:
      pausingStream
        .detectPauses(expired = 1.s, late = 501.ms, tick = 500.ms)
        .evalMap: o =>
          IO.monotonic.map(m => m.toCoarsest -> o)
        .compile.toList
        .map: result =>
          assert(result == List(
             100.ms -> InTime(1),
             200.ms -> InTime(2),
            1.s     -> Late(SyncDeadline.fromMonotonic(500.ms)),
            1500.ms -> Expired(SyncDeadline.fromMonotonic(500.ms)),
            2.s     -> Expired(SyncDeadline.fromMonotonic(500.ms)),
            2200.ms -> InTime(3),
            3.s     -> Late(SyncDeadline.fromMonotonic(2500.ms)),
            3500.ms -> Expired(SyncDeadline.fromMonotonic(2500.ms)),
            4.s     -> Expired(SyncDeadline.fromMonotonic(2500.ms)),
            4500.ms -> Expired(SyncDeadline.fromMonotonic(2500.ms)),
            5.s     -> Expired(SyncDeadline.fromMonotonic(2500.ms)),
            5200.ms -> InTime(4),
            6.s     -> Late(SyncDeadline.fromMonotonic(5500.ms)),
            6100.ms -> InTime(5)))

  "detectPauses detects initial pause" in:
    TestControl.executeEmbed:
      Stream.sleep_[IO](2.s).append(pausingStream)
        .detectPauses(expired = 1001.ms, late = 501.ms, tick = 500.ms)
        .evalMap: o =>
          IO.monotonic.map(m => m.toCoarsest -> o)
        .compile.toList
        .map: result =>
          assert(result == List(
             500.ms -> Late(SyncDeadline.fromMonotonic(0.s)),
            1.s     -> Late(SyncDeadline.fromMonotonic(0.s)),
            1500.ms -> Expired(SyncDeadline.fromMonotonic(0.s)),
            2.s     -> Expired(SyncDeadline.fromMonotonic(0.s)),
            2100.ms -> InTime(1),
            2200.ms -> InTime(2),
            3.s     -> Late(SyncDeadline.fromMonotonic(2500.ms)),
            3500.ms -> Late(SyncDeadline.fromMonotonic(2500.ms)),
            4.s     -> Expired(SyncDeadline.fromMonotonic(2500.ms)),
            4200.ms -> InTime(3),
            5.s     -> Late(SyncDeadline.fromMonotonic(4500.ms)),
            5500.ms -> Late(SyncDeadline.fromMonotonic(4500.ms)),
            6.s     -> Expired(SyncDeadline.fromMonotonic(4500.ms)),
            6500.ms -> Expired(SyncDeadline.fromMonotonic(4500.ms)),
            7.s     -> Expired(SyncDeadline.fromMonotonic(4500.ms)),
            7200.ms -> InTime(4),
            8000.ms -> Late(SyncDeadline.fromMonotonic(7500.ms)),
            8100.ms -> InTime(5)))

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
            .detectPauses(expired = 1.s, late = 100.ms, tick = 100.ms)
            .collect:
              case PauseDetector.InTime(v) => v
            .fold(0L)(_ + _)
            .headL
            .map: sum =>
              // ~30000 chunks/s on MacBook Pro M1
              logger.info(itemsPerSecondString(t.elapsed, n, "detectPauses"))
              assert(sum == (n + 1L) * (n / 2))

object PauseDetectorTest:
  private val logger = Logger[this.type]

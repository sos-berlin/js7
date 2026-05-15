package js7.base.fs2utils

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.syntax.flatMap.*
import fs2.Stream
import js7.base.catsutils.SyncDeadline
import js7.base.fs2utils.HeartbeatDetector
import js7.base.fs2utils.HeartbeatDetector.*
import js7.base.fs2utils.HeartbeatDetectorTest.*
import js7.base.log.AnsiEscapeCodes.bold
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.headL
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import scala.concurrent.duration.{Deadline, FiniteDuration}

final class HeartbeatDetectorTest extends OurAsyncTestSuite:

  private val pausingStream: Stream[IO, Int] =
    Stream.iterable(1 to 5).covary[IO].flatTap: i =>
      Stream.sleep:
        i match
          case 3 => 2.s // Long pause before the third element
          case 4 => 3.s // Long pause before the fourth element
          case 5 => 900.ms // Late abot late heartbeat
          case _ => 100.ms

  "detectHeartbeat" in :
    TestControl.executeEmbed:
      pausingStream
        .detectHeartbeat(heartbeat = 100.ms, late = 200.ms, expired = 1.s, tick = 500.ms)
        .evalMap: o =>
          IO.monotonic.map(m => m.toCoarsest -> o)
        .compile.toList
        .map: result =>
          assert(result == List(
             100.ms -> LiveValue(1, nextExpected(200.ms)),
             200.ms -> LiveValue(2, nextExpected(300.ms)),
             500.ms -> Late(nextExpected(300.ms)),
            1.s     -> Late(nextExpected(300.ms)),
            1500.ms -> Expired(nextExpected(300.ms)),
            2.s     -> Expired(nextExpected(300.ms)),
            2200.ms -> LiveValue(3, nextExpected(2300.ms)),
            2500.ms -> Late(nextExpected(2300.ms)),
            3.s     -> Late(nextExpected(2300.ms)),
            3500.ms -> Expired(nextExpected(2300.ms)),
            4.s     -> Expired(nextExpected(2300.ms)),
            4500.ms -> Expired(nextExpected(2300.ms)),
            5.s     -> Expired(nextExpected(2300.ms)),
            5200.ms -> LiveValue(4, nextExpected(5300.ms)),
            5500.ms -> Late(nextExpected(5300.ms)),
            6.s     -> Late(nextExpected(5300.ms)),
            6100.ms -> LiveValue(5, nextExpected(6200.ms))))

  "detectHeartbeat detects initial silence" in:
    TestControl.executeEmbed:
      Stream.sleep_[IO](2.s).append(pausingStream)
        .detectHeartbeat(heartbeat = 100.ms, late = 200.ms, expired = 1001.ms, tick = 500.ms)
        .evalMap: o =>
          IO.monotonic.map(m => m.toCoarsest -> o)
        .compile.toList
        .map: result =>
          assert(result == List(
             500.ms -> Late(nextExpected(0.s)),
            1.s     -> Late(nextExpected(0.s)),
            1500.ms -> Expired(nextExpected(0.s)),
            2.s     -> Expired(nextExpected(0.s)),
            2100.ms -> LiveValue(1, nextExpected(2200.ms)),
            2200.ms -> LiveValue(2, nextExpected(2300.ms)),
            2500.ms -> Late(nextExpected(2300.ms)),
            3.s     -> Late(nextExpected(2300.ms)),
            3500.ms -> Expired(nextExpected(2300.ms)),
            4.s     -> Expired(nextExpected(2300.ms)),
            4200.ms -> LiveValue(3, nextExpected(4300.ms)),
            4500.ms -> Late(nextExpected(4300.ms)),
            5.s     -> Late(nextExpected(4300.ms)),
            5500.ms -> Expired(nextExpected(4300.ms)),
            6.s     -> Expired(nextExpected(4300.ms)),
            6500.ms -> Expired(nextExpected(4300.ms)),
            7.s     -> Expired(nextExpected(4300.ms)),
            7200.ms -> LiveValue(4, nextExpected(7300.ms)),
            7500.ms -> Late(nextExpected(7300.ms)),
            8.s     -> Late(nextExpected(7300.ms)),
            8100.ms -> LiveValue(5, nextExpected(8200.ms))))

  private def nextExpected(duration: FiniteDuration) =
    SyncDeadline.fromMonotonic(duration)

  "Chunks are kept" in:
    val t = Deadline.now
    val n = 300
    val chunkSize = 10
    Stream.iterable(1 to n)
      .chunkN(chunkSize).evalMap(IO(_)).unchunks // asynchronous chunks
      .detectHeartbeat(heartbeat = 999.s, late = 999.s, expired = 999.s, tick = 999.s)
      .collect:
        case HeartbeatDetector.Alive(v) => v
      .chunks
      .compile.count
      .map: count =>
        assert(count == n / chunkSize)

  "Speed" in :
    if !sys.props.contains("test.speed") then
      IO.pure(succeed)
    else
      val n = 1_000_000
      val chunkSize = 10

      IO.defer:
        val t = Deadline.now
        Stream.iterable(1 to n)
          .chunkN(chunkSize).evalMap(IO(_)).unchunks // asynchronous chunks
          .covary[IO]
          .fold(0L)(_ + _)
          .headL
          .map: sum =>
            logger.info(bold("no operation: " + itemsPerSecondString(t.elapsed, n, "chunks")))
            assert(sum == (n + 1L) * (n / 2))
      .productR:
        IO.defer:
          val t = Deadline.now
          Stream.iterable(1 to n)
            .chunkN(chunkSize).evalMap(IO(_)).unchunks // asynchronous chunks
            .detectHeartbeat(heartbeat = 100.ms, late = 100.ms, expired = 1.s, tick = 100.ms)
            .collect:
              case HeartbeatDetector.Alive(v) => v
            .fold(0L)(_ + _)
            .headL
            .map: sum =>
              // 28'000 - 34'000 chunks/s on MacBook Pro M4
              logger.info(bold("detectHeartbeat:   " + itemsPerSecondString(t.elapsed, n / chunkSize, "chunks")))
              assert(sum == (n + 1L) * (n / 2))

object HeartbeatDetectorTest:
  private val logger = Logger[this.type]

package js7.base.log

import cats.effect.IO
import js7.base.log.Logger.syntax.*
import js7.base.log.LoggerTest.*
import js7.base.monixlike.MonixLikeExtensions.nowAsDeadline
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.RichDeadline
import js7.base.time.Stopwatch
import js7.base.time.Stopwatch.itemsPerSecondString
import org.scalatest.Assertion

final class LoggerTest extends OurAsyncTestSuite:

  if sys.props.contains("test.speed") then "Speed test" - {
    "isTraceEnabled" in:
      testSpeed("isTraceEnabled")(_ => IO.pure(logger.isTraceEnabled))

    "traceIO when trace is disabled" in:
      testSpeed("traceIO")(i => logger.traceIO(s"TEST $i")(IO.unit))

    "IO.unit" in:
      testSpeed("IO.unit", repeat = 6)(_ => IO.unit)

    def testSpeed(name: String, repeat: Int = 3)(body: Int => IO[Unit]): IO[Assertion] =
      val n = 1_000_000
      IO.defer:
        val since = ioRuntime.scheduler.nowAsDeadline()
        fs2.Stream
          .iterable(1 to n)
          .covary[IO]
          .evalMapChunk(body)
          .compile
          .drain
          .*>(IO(logger.info(itemsPerSecondString(since.elapsed, n, name))))
      .replicateA_(repeat)
      .as(succeed)
  }


object LoggerTest:
  private val logger = Logger[this.type]

package js7.base.monixlike

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.syntax.option.*
import fs2.Stream
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.*
import js7.base.monixlike.MonixLikeExtensionsTest.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Tests
import js7.base.utils.Tests.isIntelliJIdea
import scala.concurrent.TimeoutException

final class MonixLikeExtensionsTest extends OurAsyncTestSuite:

  "IO" - {
    "raceFold" - {
      "canceled" in:
        @volatile var canceled = false
        IO.never
          .onCancel(IO {
            canceled = true
            logger.info(s"raceFold canceled")
          })
          .raceFold(IO.sleep(100.ms)/*Wait for .onCancel*/)
          .map(result =>
            assert(result.getClass == classOf[Unit] && canceled))
    }

    "onErrorTap" - {
      "No error" in:
        var tapped = none[Throwable]
        for
          one <- IO(1).onErrorTap(t => IO { tapped = t.some })
        yield assert(one == 1)

      "Tapped exception" in:
        val throwable = new IllegalArgumentException
        var tapped = none[Throwable]
        for attempted <- IO.raiseError(throwable).onErrorTap(t => IO { tapped = t.some }).attempt
        yield assert(attempted.left.exists(_ eq throwable) && (tapped.get eq throwable))

      "matching PartialFunction" in:
        val throwable = new IllegalArgumentException
        var tapped = none[IllegalArgumentException]
        for
          attempted <- IO.raiseError(throwable)
            .onErrorTap:
              case t: IllegalArgumentException => IO { tapped = t.some }
            .attempt
        yield assert(attempted.left.exists(_ eq throwable) && (tapped.get eq throwable))

      "non-matching PartialFunction" in:
        val throwable = new IllegalStateException()
        var tapped = none[IllegalArgumentException]
        for
          attempted <- IO.raiseError(throwable)
            .onErrorTap:
              case t: IllegalArgumentException => IO { tapped = t.some }
            .attempt
        yield assert(attempted.left.exists(_ eq throwable) && tapped == None)
    }
  }

  "Stream" - {
    "timeoutOnSlowUpstream" - {
      def runStream(stream: Stream[IO, Any]): IO[Vector[Any]] =
        stream
          .timeoutOnSlowUpstream(3.s)
          .handleErrorWith:
            case t: TimeoutException => Stream.emit(t.getMessage)
          .compile
          .toVector

      "Timeout first" in:
        TestControl.executeEmbed:
          runStream(Stream.sleep_[IO](4.s) ++ Stream(1, 2, 3) ++ Stream(4, 5, 6))
            .map: result =>
              assert(result == Vector("timeoutOnSlowUpstream timed-out after 3s"))

      "Timeout after first chunk" in:
        TestControl.executeEmbed:
          runStream(Stream(1, 2, 3) ++ Stream.sleep_[IO](4.s) ++ Stream(4, 5, 6))
            .map: result =>
              assert(result == Vector(1, 2, 3, "timeoutOnSlowUpstream timed-out after 3s"))

      "Timeout after serval chunk" in:
        val n = if isIntelliJIdea then 10000 else 100
        TestControl.executeEmbed:
          runStream(Stream
            .iterable(1 to n)
            .flatMap(Stream.emit) // Chunk
            .evalTap(i => IO.whenA(i % 3 == 0)(IO.sleep(1.s)))
            .append(Stream.sleep_[IO](4.s)) // Timeout
            .append(Stream(4, 5, 6))
          ).map: result =>
            assert(result ==
              (1 to n).toVector :+ "timeoutOnSlowUpstream timed-out after 3s")
    }
  }

object MonixLikeExtensionsTest:
  private val logger = Logger[this.type]

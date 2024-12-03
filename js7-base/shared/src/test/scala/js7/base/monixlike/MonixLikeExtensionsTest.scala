package js7.base.monixlike

import cats.effect.{Deferred, IO}
import cats.syntax.option.*
import fs2.Stream
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.*
import js7.base.monixlike.MonixLikeExtensionsTest.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean

final class MonixLikeExtensionsTest extends OurAsyncTestSuite:

  "IO" - {
    "raceFold" - {
      "canceled" in:
        @volatile var canceled = false
        IO.never
          .onCancel(IO:
            canceled = true
            logger.info(s"raceFold canceled"))
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
        IO.raiseError(throwable)
          .onErrorTap:
            case t: IllegalArgumentException => IO:
              tapped = t.some
          .attempt
          .map: attempted =>
            assert(attempted.left.exists(_ eq throwable) && (tapped.get eq throwable))

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
    "takeUntil" in :
      pending // See takeUntilEval

    "takeUntilEval" in :
      val started = Deferred.unsafe[IO, Unit]
      val stop = Deferred.unsafe[IO, Unit]
      Stream
        .fromIterator[IO](Iterator.from(1), chunkSize = 1)
        .delayBy(1.ms)
        .evalTap: i =>
          IO.whenA(i == 3):
            started.complete(()).void
        .takeUntilEval(stop.get)
        .compile
        .count
        .both(
          started.get *> IO.sleep(100.ms) *> stop.complete(()))
        .flatMap: (n, _) =>
          IO(assert(n >= 3))

    "headL" - {
      "nonEmpty Stream" in:
        for head <- Stream(1, 2, 3).covary[IO].headL yield
          assert(head == 1)

      "empty Stream" in:
        for head <- Stream.empty.covary[IO].headL.attempt yield
          assert(head.left.toOption.get.toString ==
            "java.util.NoSuchElementException: .headL on empty stream")
    }

    "lastL" - {
      "nonEmpty Stream" in:
        for head <- Stream(1, 2, 3).covary[IO].lastL yield
          assert(head == 3)

      "empty Stream" in:
        for head <- Stream.empty.covary[IO].lastL.attempt yield
          assert(head.left.toOption.get.toString ==
            "java.util.NoSuchElementException: .lastL on empty stream")
    }
  }

  "Stream object" - {
    "unfoldChunk (similar to fromAsyncStateAction)" in:
      val list =
        Stream
          .unfold(0): i =>
            (i < 3) ? (i, i + 1)
          .toList
      assert(list == List(0, 1, 2))
  }


object MonixLikeExtensionsTest:
  private val logger = Logger[this.type]

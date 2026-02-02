package js7.base.fs2utils

import cats.effect.*
import cats.effect.kernel.Outcome.Canceled
import cats.effect.testkit.TestControl
import fs2.concurrent.SignallingRef
import fs2.{Chunk, Pipe, Pull, Stream}
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Tests.isIntelliJIdea
import scala.collection.immutable.ArraySeq
import scala.concurrent.duration.*

/** Some examples for FS2. */
final class Fs2Test extends OurAsyncTestSuite:

  "Pull" - {
    "take(n)" in:

      def myTake[F[_], A](nrOfElements: Long): Pipe[F, A, A] =
        stream =>
          def go(stream: Stream[F, A], remaining: Long): Pull[F, A, Unit] =
            if remaining <= 0 then
              Pull.done
            else
              stream.pull.uncons1.flatMap:
                case None => Pull.done
                case Some((element, remainingStream)) =>
                  Pull.output1(element) >> go(remainingStream, remaining - 1)

          go(stream, nrOfElements).stream

      val list: List[Int] = Stream.iterable(1 to 10).through(myTake(3)).toList
      assert(list == List(1, 2, 3))
      succeed

    "selectIndices(Set(2, 5, 6))" in:
      def selectIndices[F[_], A](indices: Set[Int]): Pipe[F, A, A] =
        stream =>
          def go(stream: Stream[F, A], nextList: List[Int], i: Int): Pull[F, A, Unit] =
            nextList.headOption match
              case None => Pull.done
              case Some(next) =>
                val drop = next - i
                if drop > 0 then
                  go(stream.drop(drop), nextList, i + drop)
                else
                  stream.pull.uncons1.flatMap:
                    case None => Pull.done
                    case Some((element, remainingStream)) =>
                      Pull.output1(element) >> go(remainingStream, nextList.tail, i + 1)

          go(stream, indices.to(ArraySeq).sorted.toList.dropWhile(_ < 0), 0).stream

      val list: List[String] = Stream.iterable(0 to 9).map(_.toString)
        .through(selectIndices(Set(2, 5, 6)))
        .toList
      assert(list == List("2", "5", "6"))
      succeed

    "chunkMin" in:
      val stream = Stream(1) ++ Stream(2, 3) ++ Stream(4, 5, 6) ++ Stream(7, 8, 9, 10) ++
        Stream(11, 12) ++ Stream(13) ++ Stream(14, 15)
      assert(stream.chunkMin(3).toList == List(
        Chunk(1, 2, 3), Chunk(4, 5, 6), Chunk(7, 8, 9, 10), Chunk(11, 12, 13), Chunk(14, 15)))
  }

  "Signal" - {
    if isIntelliJIdea /*time critical*/ then "interruptWhen" in:
      for
        signal <- SignallingRef[IO, Boolean](false)
        times <-
          val s1 = Stream.awakeEvery[IO](100.ms).interruptWhen(signal)
          val s2 = Stream.sleep[IO](300.ms) >> Stream.eval(signal.set(true))
          s1.concurrently(s2).compile.toVector
      yield
        assert((times: Seq[FiniteDuration]).map(d => d.toMillis / 100) == Seq(1, 2, 3))
  }

  "groupWithin" - {
    "empty" in:
      TestControl.executeEmbed:
        for
          list <- Stream.empty.covary[IO].groupWithin(3, 2.s).compile.toList
        yield
          assert(list.isEmpty)

    "0s" in:
      TestControl.executeEmbed:
        val stream = Stream(1, 2, 3, 4, 5, 6, 7) ++ Stream.sleep_[IO](1.s) ++ Stream(8)
        for
          list <- stream.groupWithin(3, 0.s).compile.toList
        yield
          assert(list == List(
            Chunk(1, 2, 3),
            Chunk(4, 5, 6),
            Chunk(7), // Small chunks are preserved !!!
            Chunk(8)))

    "standard" in:
      TestControl.executeEmbed:
        val stream = Stream(1, 2, 3, 4)
          .covary[IO]
          .append:
            Stream.sleep[IO](1.s) >> Stream(5, 6, 7)
          .append:
            Stream.sleep[IO](3.s) >> (Stream(8) ++ Stream.empty)
          .append:
            Stream.sleep[IO](10.s) >> Stream(9, 10, 11, 12, 13, 14, 15)
          .append:
            Stream.sleep[IO](3.s) >> Stream(16, 17, 18, 19, 20, 21, 22)
        for
          list <- stream.groupWithin(3, 2.s).evalMap(o => IO.monotonic.map(_.toCoarsest -> o)).compile.toList
        yield
          assert(list == List(
            0.s -> Chunk(1, 2, 3),
            1.s -> Chunk(4, 5, 6),
            3.s -> Chunk(7),
            5.s/*6s?*/ -> Chunk(8),
            14.s -> Chunk(9, 10, 11),
            14.s -> Chunk(12, 13, 14),
            16.s -> Chunk(15),
            17.s -> Chunk(16, 17, 18),
            17.s -> Chunk(19, 20, 21),
            17.s -> Chunk(22)))
  }

  "Beware of Chunk[Char]#asSeq!" - {
    val charChunk: Chunk[Char] = Chunk.array("abc".toCharArray)

    "Chunk[Char]" in :
      charChunk match
        case Chunk.ArraySlice(array, 0, 3) => assert(array(0) == 'a')
        case _ => fail("ArraySlice expected")

    "💥 .asSeq.grouped(3) yield a unexpectedly typed Chunk" in :
      val chunk: Chunk[Char] = Chunk.from(charChunk.asSeq.grouped(3).next())
      val e = intercept[ClassCastException]:
        chunk match
          case Chunk.ArraySlice(_, _, _) =>
          case _ => fail("ArraySlice expected")
      assert(e.getMessage == "class [Ljava.lang.Object; cannot be cast to class [C" +
        " ([Ljava.lang.Object; and [C are in module java.base of loader 'bootstrap')")

    ".toArray instead of .asSeq" in :
      val chunk: Chunk[Char] = Chunk.from(charChunk.toArray.grouped(3).next())
      chunk match
        case Chunk.ArraySlice(_, _, _) => succeed
        case _ => fail("ArraySlice expected")
  }

  "interruptWhen cancels upstream and downstream IOs !!" - {
    "interruptWhen cancels the upstream" in:
      for
        signal <- SignallingRef[IO].of(false)
        deferred <- Deferred[IO, OutcomeIO[Unit]]
        stream = Stream.unit
          .evalMap: _ =>
            // Cancelled due to interruptWhen
            IO.sleep(3.s).guaranteeCase(deferred.complete(_).void)
          .interruptWhen(signal)
        fiber <- stream.compile.toList.timed.start
        _ <- IO.sleep(100.ms) // Be sure, the stream has started sleeping
        _ <- signal.set(true)
        (duration, result) <- fiber.joinStd
        outcome <- deferred.get
      yield
        assert(result.isEmpty & outcome == Outcome.Canceled() & (duration < 1.s))

    "interruptWhen cancels the downstream, too (!)" in:
      for
        signal <- SignallingRef[IO].of(false)
        deferred <- Deferred[IO, OutcomeIO[Unit]]
        stream = Stream.unit
          .interruptWhen(signal)
          .evalMap: _ =>
            // Cancelled due to interruptWhen
            IO.sleep(3.s).guaranteeCase:
              deferred.complete(_).void
        fiber <- stream.compile.toList.timed.start
        _ <- IO.sleep(100.ms) // Be sure, the stream has started sleeping
        _ <- signal.set(true)
        (duration, result) <- fiber.joinStd
        outcome <- deferred.get
      yield
        assert(result.isEmpty & outcome == Outcome.Canceled() & (duration < 1.s))

    "interruptWhen doesn't cancel a merged-in stream" in:
      for
        signal <- SignallingRef[IO].of(false)
        deferred <- Deferred[IO, OutcomeIO[String]]
        stream = Stream.unit
          .interruptWhen(signal)
          .merge:
            // Not cancelled
            Stream.eval:
              IO.sleep(300.ms).as("OK").guaranteeCase:
                deferred.complete(_).void
        fiber <- stream.compile.toList.timed.start
        _ <- IO.sleep(100.ms) // Be sure, the stream has started sleeping
        _ <- signal.set(true)
        (duration, result) <- fiber.joinStd
        outcome <- deferred.get
      yield
        assert(result == List((), "OK") & outcome.isSuccess & (duration >= 300.ms))

    "interruptWhen doesn't cancel a downstream after *prefetch*" in:
      for
        signal <- SignallingRef[IO].of(false)
        deferred <- Deferred[IO, OutcomeIO[String]]
        stream = Stream.unit
          .interruptWhen(signal)
          .prefetch // Only upstream is interrupted (cancelled)
          .evalMap: _ =>
            IO.sleep(300.ms).as("OK").guaranteeCase:
              deferred.complete(_).void
        fiber <- stream.compile.toList.timed.start
        _ <- IO.sleep(100.ms) // Be sure, the stream has started sleeping
        _ <- signal.set(true)
        (duration, result) <- fiber.joinStd
        outcome <- deferred.get
      yield
        assert(result == List("OK") & outcome.isSuccess & (duration >= 300.ms))

    "interruptWhen doesn't cancel a downstream after *prefetch* (2)" in:
      pending // ???
      for
        signal <- SignallingRef[IO].of(false)
        deferred <- Deferred[IO, OutcomeIO[String]]
        stream = Stream.unit[IO]
          .prefetch // Only upstream is interrupted (cancelled)
          .evalMap: _ =>
            IO.sleep(300.ms).as("OK").guaranteeCase:
              deferred.complete(_).void
          .interruptWhen(signal)
        fiber <- stream.compile.toList.timed.start
        _ <- IO.sleep(100.ms) // Be sure, the stream has started sleeping
        _ <- signal.set(true)
        (duration, result) <- fiber.joinStd
        outcome <- deferred.get
      yield
        assert(result == List("OK") & outcome.isSuccess & (duration >= 300.ms))
  }

  if false then // Manual test. Start with -Xmx10m
    "Recursive streams doesn't eat heap" in:
      def f(i: Int): Stream[IO, Int] =
        var last = i
        Stream.emit(i)
          .covary[IO]
          .map: i =>
            last = i
            if i % 1_000_000 == 0 then Logger.info(s"$i")
            i
          .append:
            f(last + 1)
      f(0).last.compile.drain.as(succeed)

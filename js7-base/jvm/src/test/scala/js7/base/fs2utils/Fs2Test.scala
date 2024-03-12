package js7.base.fs2utils

import cats.effect.*
import fs2.concurrent.SignallingRef
import fs2.{Chunk, Pipe, Pull, Stream}
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Tests
import js7.base.utils.Tests.isIntelliJIdea
import scala.collection.immutable.ArraySeq
import scala.concurrent.Future
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

    "chunkMin" in :
      val stream = Stream(1) ++ Stream(2, 3) ++ Stream(4, 5, 6) ++ Stream(7, 8, 9, 10) ++
        Stream(11, 12) ++ Stream(13) ++ Stream(14, 15)
      assert(stream.chunkMin(3).toList == List(
        Chunk(1, 2, 3), Chunk(4, 5, 6), Chunk(7, 8, 9, 10), Chunk(11, 12, 13), Chunk(14, 15)))
  }

  "Signal" - {
    if isIntelliJIdea /*time critical*/ then "interruptWhen" in :
      for
        signal <- SignallingRef[IO, Boolean](false)
        times <-
          val s1 = Stream.awakeEvery[IO](100.ms).interruptWhen(signal)
          val s2 = Stream.sleep[IO](300.ms) >> Stream.eval(signal.set(true))
          s1.concurrently(s2).compile.toVector
      yield assert((times: Seq[FiniteDuration]).map(d => d.toMillis / 100) == Seq(1, 2, 3))
  }

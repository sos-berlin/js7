package js7.base.fs2utils

import cats.effect.{Deferred, IO}
import fs2.{Pure, Stream}
import js7.base.fs2utils.StreamExtensions.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*

final class StreamExtensionsTest extends OurAsyncTestSuite:

  "+:" in:
    val intStream: Stream[Pure, Int] = 1 +: Stream(2, 3)
    assert(intStream.compile.to(Seq) == Seq(1, 2, 3))

    val stream = ("one" +: Stream[Pure, Int](2, 3))
    assert(stream.toList == List("one", 2, 3))

  "prependOne" in:
    val intStream: Stream[Pure, Int] = Stream(2, 3).prependOne(1)
    assert(intStream.compile.to(Seq) == Seq(1, 2, 3))

    val anyStream = Stream(2, 3).prependOne("one")
    assert(anyStream.toList == List("one", 2, 3))

  "takeUntil" in:
    pending // See takeUntilEval

  "takeUntilEval" in:
    val started = Deferred.unsafe[IO, Unit]
    val stop = Deferred.unsafe[IO, Unit]
    Stream
      .fromIterator[IO](Iterator.from(1), chunkSize = 1)
      .delayBy(1.ms)
      .evalTap(i => IO.whenA(i == 3)(started.complete(()).void))
      .takeUntilEval(stop.get)
      .compile
      .count
      .both(
        started.get *> IO.sleep(100.ms) *> stop.complete(()))
      .flatMap((n, _) => IO(assert(n >= 3)))

  "onStart" in:
    val subscribed = Atomic(0)
    val stream: Stream[IO, Int] =
      Stream(1, 2, 3)
        .covary[IO]
        .onStart:
          IO:
            subscribed += 1

    for
      a <- stream.compile.toList
      b <- stream.compile.toList
      _ <- IO(assert(subscribed.get == 2 && a == List(1, 2, 3) && a == b))
    yield succeed

  "onErrorEvalTap" in:
    val throwable = new Exception("TEST")
    var catched: Throwable = null
    for
      _ <- Stream.eval(IO(1)).onErrorEvalTap(t => IO.unit).compile.drain
      result <- Stream
        .raiseError[IO](throwable)
        .onErrorEvalTap(t => IO { catched = t })
        .compile.drain
        .attempt
    yield
      val Left(t) = result: @unchecked
      assert((t eq throwable) && (catched eq throwable))

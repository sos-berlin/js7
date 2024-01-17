package js7.base.fs2utils

import cats.effect.testkit.TestControl
import cats.effect.{Deferred, IO}
import fs2.concurrent.SignallingRef
import fs2.{Chunk, Pure, Stream}
import js7.base.catsutils.CatsDeadline
import js7.base.fs2utils.StreamExtensions.*
import js7.base.fs2utils.StreamExtensionsTest.*
import js7.base.log.Logger
import js7.base.test.{OurAsyncTestSuite, TestCatsEffect}
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

  "onlyNewest" - {
    "onlyNewest" in:
      val stream: Stream[IO, Int] =
        Stream.sleep_[IO](1.s) ++ Stream(1) ++ Stream(2, 3) ++
          Stream.sleep_[IO](1.s) ++ Stream(4, 5) ++ Stream(6) ++
          Stream.sleep_[IO](1.s) ++ Stream(7) ++ Stream(8, 9)

      val program =
        for
          result <- stream.onlyNewest.evalTap(_ => IO.sleep(100.ms)).compile.toList
        yield
          assert(result == List(1, 3, 5, 6, 7, 9))

      TestControl.executeEmbed(program, seed = Some(TestCatsEffect.toSeed(1)))

    "Error handling" in:
      val e = new IllegalStateException("TEST ERROR")
      TestControl.executeEmbed:
        for
          result <- Stream.raiseError[IO](e).covaryOutput[Long].onlyNewest.attempt.compile.toList
        yield
          assert(result.size == 1 && result.head.left.toOption.get.eq(e))
  }

  //"splitBigByteSeqs" in:
  //    val maxSize = 2
  //    val strings = Vector.fill(100_000)(
  //      ('A' + Random.nextInt(26)).toChar.toString * Random.nextInt(maxSize + 1) + "\n")
  //    val expectedCount = strings.view.map(o => (o.size + maxSize - 1) / maxSize).sum
  //    val result = Stream
  //      .iterable(strings)
  //      .map(ByteArray(_))
  //      .splitBigByteSeqs(maxSize)
  //      .evalTap: byteSeq =>
  //        assert(byteSeq.length <= maxSize)
  //      .compile.toList
  //    assert(result.length == expectedCount)
  //    assert(result.view.map(_.utf8String).mkString == strings.mkString)

  "echoRepeated" - {
    "Empty" in:
      TestControl.executeEmbed:
        for result <- Stream.empty.covary[IO].echoRepeated(3.s).compile.toList yield
          assert(result.isEmpty)

    "Empty Chunk" in:
      TestControl.executeEmbed:
        for result <- Stream.chunk(Chunk.empty).covary[IO].echoRepeated(3.s).compile.toList yield
          assert(result.isEmpty)

    lazy val stream: Stream[IO, Int] =
      Stream.sleep_[IO](100.s).append(Stream(1, 2))
        .append(Stream.sleep_[IO](1.s)).append(Stream(3, 4))
        .append(Stream.sleep_[IO](7.s)).append(Stream(5))
        .append(Stream.sleep_[IO](4.s)).append(Stream(6))
        .append(Stream.sleep_[IO](1.s)).append(Stream.empty)
        .append(Stream.sleep_[IO](1.s)).append(Stream(7))
        .evalTap(a => IO(logger.info(s"echoRepeated: source stream a=$a")))
        .echoRepeated(3.s)

    "Source terminates" in:
      TestControl.executeEmbed:
        for result <- stream.compile.toList yield
          assert(result == List(1, 2, 3, 4, 4, 4, 5, 5, 6, 7))

    "Destination terminates" in:
      TestControl.executeEmbed:
        for result <- stream.take(5).compile.toList yield
          assert(result == List(1, 2, 3, 4, 4))

    "Failing source" in:
      val exception = new IllegalArgumentException("TEST")
      val stream = Stream
        .emit(1)
        .append(Stream.sleep_[IO](4.s)).appendOne(2)
        .append(Stream.raiseError[IO](exception))
        .appendOne(-888)
        .echoRepeated(3.s)

      TestControl.executeEmbed:
        for
          since <- CatsDeadline.now
          result <- stream.attempt.evalMap(o => since.elapsed.map(_ -> o)).compile.toList
        yield
          assert(result == List(
            0.s -> Right(1),
            3.s -> Right(1),
            4.s -> Right(2),
            4.s -> Left(exception)))

    "Cancel" in:
      TestControl.executeEmbed:
        for
          signal <- SignallingRef.of[IO, Boolean](false)
          result <- IO.both(
            stream.interruptWhen(signal).compile.toList,
            signal.set(true).delayBy(100.s + 5.s))
        yield
          assert(result == List(1, 2, 3, 4, 4) -> ())
  }

  "insertHeartbeatsOnSlowUpstream" - {
    val heartbeat = -99

    "Empty" in :
      TestControl.executeEmbed:
        for
          result <- Stream.empty.covary[IO]
            .insertHeartbeatsOnSlowUpstream(3.s, heartbeat)
            .compile.toList
        yield assert(result.isEmpty)

    lazy val stream: Stream[IO, Int] =
      Stream.sleep_[IO](7.s).append(Stream(1, 2))
        .append(Stream.sleep_[IO](1.s)).append(Stream(3, 4))
        .append(Stream.sleep_[IO](7.s)).appendOne(5)
        .append(Stream.sleep_[IO](4.s)).appendOne(6)
        .append(Stream.sleep_[IO](1.s)).append(Stream.empty)
        .append(Stream.sleep_[IO](1.s)).appendOne(7)
        .evalTap(a => IO(logger.info(s"insertHeartbeatsOnSlowUpstream: source stream a=$a")))
        .insertHeartbeatsOnSlowUpstream(3.s, heartbeat)

    "Source terminates" in:
      TestControl.executeEmbed:
        for
          since <- CatsDeadline.now
          result <- stream.evalMap(o => since.elapsed.map(_ -> o)).compile.toList
        yield
          assert(result == List(
             3.s -> heartbeat,
             6.s -> heartbeat,
             7.s -> 1,
             7.s -> 2,
             8.s -> 3,
             8.s -> 4,
            11.s -> heartbeat,
            14.s -> heartbeat,
            15.s -> 5,
            18.s -> heartbeat,
            19.s -> 6,
            21.s -> 7))

    "Destination terminates" in:
      TestControl.executeEmbed:
        for result <- stream.take(5).compile.toList yield
          assert(result == List(heartbeat, heartbeat, 1, 2, 3))

    "Failing source" in:
      val exception = new IllegalArgumentException("TEST")
      val stream = Stream
        .emit(1)
        .append(Stream.sleep_[IO](4.s)).appendOne(2)
        .append(Stream.raiseError[IO](exception))
        .appendOne(-888)
        .insertHeartbeatsOnSlowUpstream(3.s, heartbeat)

      TestControl.executeEmbed:
        for
          since <- CatsDeadline.now
          result <- stream.attempt.evalMap(o => since.elapsed.map(_ -> o)).compile.toList
        yield
          assert(result == List(
            0.s -> Right(1),
            3.s -> Right(heartbeat),
            4.s -> Right(2),
            4.s -> Left(exception)))

    "Cancel" in:
      TestControl.executeEmbed:
        for
          signal <- SignallingRef.of[IO, Boolean](false)
          result <- IO.both(
            stream.interruptWhen(signal).compile.toList,
            signal.set(true).delayBy(7.s + 5.s))
        yield
          assert(result == List(heartbeat, heartbeat, 1, 2, 3, 4, heartbeat) -> ())
  }

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


object StreamExtensionsTest:
  private val logger = Logger[this.type]

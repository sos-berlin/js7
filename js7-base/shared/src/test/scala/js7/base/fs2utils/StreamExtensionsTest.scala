package js7.base.fs2utils

import cats.effect.kernel.Resource.ExitCase
import cats.effect.testkit.TestControl
import cats.effect.{IO, Resource, SyncIO}
import fs2.concurrent.SignallingRef
import fs2.{Chunk, Pure, Stream}
import js7.base.catsutils.CatsDeadline
import js7.base.fs2utils.StreamExtensions.*
import js7.base.fs2utils.StreamExtensionsTest.*
import js7.base.log.Logger
import js7.base.test.{OurAsyncTestSuite, TestCatsEffect}
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.{Atomic, SetOnce}
import scala.concurrent.duration.FiniteDuration

final class StreamExtensionsTest extends OurAsyncTestSuite:

  "Stream" - {
    "+:" in:
      val intStream: Stream[Pure, Int] = 1 +: Stream(2, 3)
      assert(intStream.compile.to(Seq) == Seq(1, 2, 3))

      val stream = ("one" +: Stream[Pure, Int](2, 3))
      assert(stream.toList == List("one", 2, 3))

    "prepend" in:
      val intStream: Stream[Pure, Int] = Stream(3, 4).prepend(Stream(1, 2))
      assert(intStream.compile.to(Seq) == Seq(1, 2, 3, 4))

      val anyStream = Stream(3, 4).prepend(Stream("one", "two"))
      assert(anyStream.toList == List("one", "two", 3, 4))

      locally:
        val stream = Stream(1, 2)
        val empty = Stream.empty
        val result = stream.prepend(empty)
        assert(result.toList == stream.toList)
        //does not compile: assert(Stream(1, 2).prepend(Stream.empty) == Stream(1, 2))

    "prependOne" in:
      val intStream = Stream(2, 3).prependOne(1)
      assert(intStream.toList == List(1, 2, 3))

      val anyStream = Stream(2, 3).prependOne("one")
      assert(anyStream.toList == List("one", 2, 3))

    "appendOne" in:
      val intStream = Stream(1, 2).appendOne(3)
      assert(intStream.toList == List(1, 2, 3))

      val anyStream = Stream(1, 2).appendOne("three")
      assert(anyStream.toList == List(1, 2, "three"))

    "fillUpChunks" in:
      val stream: Stream[Pure, Int] = Stream(1) ++ Stream(2, 3) ++ Stream(4, 5, 6) ++ Stream(7, 8, 9, 10) ++
        Stream(11, 12) ++ Stream(13) ++ Stream(14, 15)
      val filledUp = stream.chunkMin(3)
      assert(filledUp.toList == List(
        Chunk(1, 2, 3), Chunk(4, 5, 6), Chunk(7, 8, 9, 10), Chunk(11, 12, 13), Chunk(14, 15)))

      // Does not compile ???
      //  Found:    (stream : fs2.Stream[fs2.Pure, Int])
      //  Required: fs2.Stream[Nothing, Int]
      //assert(stream.fillUpChunks(limit = 3).chunks.toList == List(
      //  Chunk(1, 2, 3), Chunk(4, 5, 6), Chunk(7, 8, 9, 10), Chunk(11, 12, 13), Chunk(14, 15)))

    "onStart" in:
      val subscribed = Atomic(0)
      val stream: Stream[IO, Int] =
        Stream(1, 2, 3).covary[IO]
          .onStart:
            IO:
              subscribed += 1
      for
        a <- stream.compile.toList
        b <- stream.compile.toList
        _ <- IO(assert(subscribed.get == 2 && a == List(1, 2, 3) && a == b))
      yield succeed

    "evalTapFirst" in:
      val first = SetOnce[Int]
      val list = Stream(1, 2, 3).covary[SyncIO]
        .evalTapFirst(o => SyncIO(first := o))
        .compile
        .toList
        .unsafeRunSync()
      assert(list == List(1, 2, 3) && first.orThrow == 1)

    "onlyNewest" - {
      "onlyNewest" in:
        val stream: Stream[IO, Int] =
          Stream.sleep_[IO](1.s) ++ Stream(1) ++ Stream.empty ++ Stream(2, 3) ++
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
            result <- Stream
              .raiseError[IO](e)
              .covaryOutput[Long]
              .onlyNewest
              .attempt
              .compile.toList
          yield
            assert(result.size == 1 && result == List(Left(e)))
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

    "addAfterIdle" in:
      TestControl.executeEmbed:
        val silence = 3.s
        val stream = Stream(
          Chunk("A", "B"),
          2.s, Chunk("C"), Chunk("D"),
          2.s, Chunk("E", "F", "G"),
          3.s, Chunk("H"),
          10.s, Chunk("I", "J"),
          1.s, Chunk("K"))
        stream
          .flatMap:
            case d: FiniteDuration => Stream.sleep_[IO](d)
            case o: Chunk[String] => Stream.emit(o)
          .unchunks
          .addAfterIdle(silence, IO.pure("TIMEOUT"))
          .chunks
          .compile.toVector
          .map: result =>
            assert(result == Vector(
              Chunk("A", "B"),
              Chunk("C"),
              Chunk("D"),
              Chunk("E", "F", "G"),
              Chunk("TIMEOUT"),
              Chunk("H"),
              Chunk("TIMEOUT"),
              Chunk("I", "J"),
              Chunk("K")))

    "collectAndFlushOnSilence" in:
      TestControl.executeEmbed:
        val silence = 3.s
        val stream = Stream(
          "A", "B",
          2.s, "C",
          2.s, "D", "E", "F",
          3.s, "G",
          4.s, "H", "I",
          1.s, "J")
        stream
          .covary[IO]
          .flatMap:
            case d: FiniteDuration => Stream.sleep_[IO](d)
            case o: String => Stream(o)
          .collectAndFlushOnSilence(silence)
          .compile.toVector
          .map: result =>
            assert(result == Vector(
              Chunk("A", "B", "C", "D", "E", "F"),
              Chunk("G"),
              Chunk("H", "I", "J")))

    "chunkWithin" - {
      "empty" in:
        TestControl.executeEmbed:
          for
            list <- Stream.empty.covary[IO].chunkWithin(3, 2.s).compile.toList
          yield
            assert(list.isEmpty)

      "0s" in:
        TestControl.executeEmbed:
          val stream = Stream(1, 2, 3, 4, 5, 6, 7) ++ Stream.sleep_[IO](1.s) ++ Stream(8)
          for
            list <- stream.chunkWithin(3, 0.s).compile.toList
          yield
            assert(list == List(Chunk(1, 2, 3), Chunk(4, 5, 6), Chunk(7, 8)))

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
            list <- stream.chunkWithin(3, 2.s).evalMap(o => IO.monotonic.map(_.toCoarsest -> o)).compile.toList
          yield
            assert(list == List(
              0.s -> Chunk(1, 2, 3),
              1.s -> Chunk(4, 5, 6),
              3.s -> Chunk(7),
              6.s -> Chunk(8),
              14.s -> Chunk(9, 10, 11),
              14.s -> Chunk(12, 13, 14),
              16.s -> Chunk(15),
              17.s -> Chunk(16, 17, 18),
              17.s -> Chunk(19, 20, 21),
              17.s -> Chunk(22)))
    }

    "repeatLast" in:
      val eternal = Stream(1, 2, 3).repeatLast
      assert(eternal.take(7).toList == List(1, 2, 3, 3, 3, 3, 3))

      val empty = Stream.empty.repeatLast
      assert(empty.toList == Nil)

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

    "mapParallelBatch" - {
      val n = 1009 // Prime number
      val nRange = 1 to n

      "Empty" in:
        for
          list <- Stream.empty.covaryAll[IO, Int].mapParallelBatch()(_ + 1).compile.toList
        yield
          assert(list.isEmpty)

      "Simple case" in:
        for
          list <- Stream.iterable(nRange)
            .chunkN(3).unchunks
            .covary[IO]
            .mapParallelBatch()(_ + 1)
            .compile.toList
        yield
          assert(list == List.from(nRange.map(_ + 1)))

      "Early terminated stream" in:
        val limit = 919 // prime number
        val stream = Stream.iterable(nRange) ++ Stream.never[IO] ++ Stream(1010)
        for
          list <- stream.mapParallelBatch()(_ + 1).take(limit).compile.toList
        yield
          assert(list == List.from(nRange.map(_ + 1).take(limit)))
    }

    "logTiming" in:
      val n = 7777
      var duration: FiniteDuration = null
      var count: Long = 0
      var exitCase: ExitCase = null
      Stream
        .iterable(0 until n)
        .covary[IO]
        .logTiming(_ => 2, (d, n, e) => IO:
          duration = d
          count = n
          exitCase = e)
        .compile.drain
        .map: _ =>
          assert(duration.isPositive && count == 2 * n && exitCase == ExitCase.Succeeded)

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
  }

  "Chunk" - {
    "convertToString ArraySlice" in:
      val chunk = Chunk.array("abc".toCharArray)
      val arraySlice = chunk.asInstanceOf[Chunk.ArraySlice[Char]]
      assert(arraySlice == Chunk.ArraySlice(Array('a', 'b', 'c'), 0, 3))
      assert(chunk.convertToString == "abc")

    "convertToString other" in:
      val chunk: Chunk[Char] =
        Chunk.array("abc".toCharArray) ++ Chunk.array("def".toCharArray) ++ Chunk.singleton('g')
      chunk.asInstanceOf[Chunk.Queue[Char]]
      assert(chunk.convertToString == "abcdefg")

    "fromString" in:
      val chunk = Chunk.fromString("abc")
      assert(chunk == Chunk('a', 'b', 'c'))
      val string = chunk.convertToString
      assert(string == "abc")

    "grouped" in:
      val chunk: Chunk[Char] = Chunk.fromString("0123456789")
      val iterator: Iterator[Chunk[Char]] = chunk.grouped(3)
      assert(Chunk.iterator(iterator) == Chunk(
        Chunk('0', '1', '2'),
        Chunk('3', '4', '5'),
        Chunk('6', '7', '8'),
        Chunk('9')))
  }

object StreamExtensionsTest:
  private val logger = Logger[this.type]

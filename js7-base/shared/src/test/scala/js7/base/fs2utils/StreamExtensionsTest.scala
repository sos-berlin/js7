package js7.base.fs2utils

import cats.effect.Resource.ExitCase
import cats.effect.testkit.TestControl
import cats.effect.{IO, Resource, SyncIO}
import fs2.{Chunk, Pure, Stream}
import js7.base.fs2utils.StreamExtensions.*
import js7.base.fs2utils.StreamExtensionsTest.*
import js7.base.log.AnsiEscapeCodes.bold
import js7.base.log.Logger
import js7.base.test.{OurAsyncTestSuite, TestCatsEffect}
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.Tests.isIntelliJIdea
import js7.base.utils.{Atomic, SetOnce}
import scala.concurrent.duration.*

final class StreamExtensionsTest extends OurAsyncTestSuite:

  "Stream" - {
    "+:" in:
      val intStream: Stream[Pure, Int] = 1 +: Stream(2, 3)
      assert(intStream.compile.to(Seq) == Seq(1, 2, 3))

      val stream = "one" +: Stream[Pure, Int](2, 3)
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

    "chunkWeighted" - {
      "empty stream" in :
        val strings = Stream.empty.covaryOutput[String]
        val grouped = strings.chunkWeighted(10)(_.length)
        assert(grouped.toList.isEmpty)

      "single string exceeding limit starts its own chunk" in :
        val limit = 5
        val strings = Stream("abcdefgh")
        val grouped = strings.chunkWeighted(limit)(_.length)
        assert(grouped.toList == List(Chunk("abcdefgh")))

      "group strings by length" in :
        val limit = 10
        val strings = Stream("a", "bb", "ccc", "dddd", "eeeee", "ffffff") ++ Stream("xxxx", "yyyyyyyy")

        val grouped: Stream[Pure, Chunk[String]] =
          strings.chunkWeighted(limit)(_.length)

        val result = grouped.toList

        // "a" (1), "bb" (2), "ccc" (3), "dddd" (4) -> sum 10 <= 10. Chunk("a", "bb", "ccc", "dddd")
        // "eeeee" (5), "ffffff" (6) -> sum 11 > 10.
        //   "eeeee" (5) starts new chunk.
        //   "ffffff" (6) -> 5 + 6 = 11 > 10. "ffffff" starts new chunk.

        assert(result == List(
          Chunk("a", "bb", "ccc", "dddd"),
          Chunk("eeeee"),
          Chunk("ffffff", "xxxx"),
          Chunk("yyyyyyyy")))
    }

    "mapChunkWeighted avoids high memory usage – manual test" in :
      if !isIntelliJIdea then
        pending
      else
        IO.defer:
          logger.info(s"totalMemory=${toKiBGiB(sys.runtime.totalMemory)}")
          val elemSize: Int = 250 * 1024
          val charChunkSize = 1024 * 1024
          //val n = BigDecimal(sys.runtime.totalMemory / charChunkSize).toIntExact
          fs2.Stream.iterable(0 to charChunkSize by 1024)
            .covary[IO]
            .mapChunkWeighted[String]("+" * _, charChunkSize)(_.length)
            .map: chunk =>
              logger.info(s"chunk count=${chunk.size} · ${toKiBGiB(chunk.asSeq.map(_.length).sum)} = ${chunk.asSeq.map(o => toKiBGiB(o.length)).mkString(" + ")}")
              val size = chunk.iterator.map(_.length).sum
              assert(size >= elemSize & size <= charChunkSize)
              chunk
            .compile.drain.as(succeed)

    "combineWeightedInChunk" - {
      "empty stream" in :
        val strings = Stream.empty.covaryOutput[String]
        val grouped = strings.combineWeightedInChunk(10)(_.length)
        assert(grouped.toList.isEmpty)

      "single string exceeding limit starts its own chunk" in :
        val limit = 5
        val strings = Stream("abcdefgh")
        val grouped = strings.combineWeightedInChunk(limit)(_.length)
        assert(grouped.toList == List("abcdefgh"))

      "group strings by length with limit" in :
        val limit = 5
        val strings = Stream("a", "bb", "ccc", "dddd", "eeeee", "ffffff") ++ Stream("xxxx", "yyyyyyyy")

        val grouped: Stream[Pure, String] =
          strings.combineWeightedInChunk(limit)(_.length)

        assert(grouped.toList == List("abb", "ccc", "dddd", "eeeee", "ffffff", "xxxx", "yyyyyyyy"))

      "respects chunk boundaries" in :
        val limit = 5
        val stream = Stream("a", "bb") ++
          Stream("cc", "ddd") ++
          Stream("e") ++
          Stream("fffff") ++
          Stream("gggggg", "hhhh")

        val grouped = stream.combineWeightedInChunk(limit)(_.length)
        val result = grouped.toList

        assert(result == List(
          "abb", "ccddd",
          "e",
          "fffff",
          "gggggg", "hhhh"))
    }

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

    "onFirst" in:
      val first = SetOnce[Int]
      val list = Stream(1, 2, 3).covary[SyncIO]
        .onFirst(o => SyncIO(first := o))
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
      var duration: FiniteDuration = null.asInstanceOf[FiniteDuration]
      var count: Long = 0
      var exitCase: ExitCase = null.asInstanceOf[ExitCase]
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
      var catched: Throwable = null.asInstanceOf[Throwable]
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

    "collectNonNull" in:
      val list: List[Int] =
        Stream[Pure, Int | Null](1, 2, 3, null, 4, 5)
          .collectNonNull
          .toList
      assert(list == List(1, 2, 3, 4, 5))

    "takeUntil" in:
      val list: List[Int] =
        Stream(1, 2, 3, 4, 5).takeUntil(_ == 3).toList
      assert(list == List(1, 2, 3))

    "takeWhileNotNull" in:
      val list: List[Int] =
        Stream[Pure, Int | Null](1, 2, 3, null, 4, 5)
          .takeWhileNotNull
          .toList
      assert(list == List(1, 2, 3))
  }

  "Chunk" - {
    "convertToString" - {
      "ArraySlice" in:
        val chunk = Chunk.array("abc".toCharArray)
        val arraySlice = chunk.asInstanceOf[Chunk.ArraySlice[Char]]
        assert(arraySlice == Chunk.ArraySlice(Array('a', 'b', 'c'), 0, 3))
        assert(chunk.convertToString == "abc")

      "Other Chunk" in:
        val chunk: Chunk[Char] =
          Chunk.array("abc".toCharArray) ++ Chunk.array("def".toCharArray) ++ Chunk.singleton('g')
        chunk.asInstanceOf[Chunk.Queue[Char]]
        assert(chunk.convertToString == "abcdefg")
    }

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

     "byteAt" in:
        val chunk = Chunk.array(Array[Byte](0, 1 ,2))
        (1 to 3).foreach: i =>
          assert(chunk.byteAt(0) == chunk(0))
        succeed
  }


  "Speed" - {
    "Chunk[Byte] byteAt" in:
      if isIntelliJIdea then
        val n = 1_000_000_000
        val size = 128

        (1 to 3).foreach: _ =>
          val chunk: Chunk[Byte] = Chunk.array(Array.fill[Byte](size)(7))
          val t = Deadline.now
          var i = 0
          while i < n do
            chunk.byteAt(i & 0x7f) // Same speed as boxing apply(i), <= 1ns
            i += 1
          val d = t.elapsed
          logger.info(bold(s"${itemsPerSecondString(d, n, "byteAt(i)")}"))

        (1 to 3).foreach: _ =>
          val chunk: Chunk[Byte] = Chunk.array(Array.fill[Byte](size)(7))
          val t = Deadline.now
          var i = 0
          while i < n do
            chunk(i & 0x7f)
            i += 1
          val d = t.elapsed
          logger.info(bold(s"${itemsPerSecondString(d, n, "apply(i)")}"))
      succeed
  }

object StreamExtensionsTest:
  private val logger = Logger[this.type]

package js7.common.jsonseq

import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Json}
import java.io.FileOutputStream
import java.nio.file.Files
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.io.file.FileUtils.implicits.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{touchFile, withTemporaryFile}
import js7.base.test.OurTestSuite
import js7.base.time.{Stopwatch, Timestamp}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.jsonseq.FileJsonSeqTest.*
import js7.data.event.{Event, KeyedEvent, KeyedEventTypedJsonCodec, Stamped}

/**
  * @author Joacim Zschimmer
  */
final class FileJsonSeqTest extends OurTestSuite:

  "Empty file" in:
    withTemporaryFile { file =>
      touchFile(file)
      autoClosing(InputStreamJsonSeqReader.open(file)) { reader =>
        assert(reader.read().isEmpty)
      }
    }

  "File with no JSON document is empty" in:
    withTemporaryFile { file =>
      autoClosing(new OutputStreamJsonSeqWriter(new FileOutputStream(file))) { _ => }
      assert(Files.size(file) == 0)
      autoClosing(InputStreamJsonSeqReader.open(file)) { reader =>
        assert(reader.read().isEmpty)
      }
    }

  "Some JSON document" in:
    withTemporaryFile { file =>
      autoClosing(new OutputStreamJsonSeqWriter(new FileOutputStream(file))) { w =>
        w.writeJson(A(1, "a").asJson)
        w.writeJson(A(2, "b").asJson)
        w.writeJson(A(3, "c").asJson)
        w.flush()
      }
      //assert(file.contentString startsWith Ascii.RS.toChar.toString)
      assert(file.contentString.endsWith("\n"))
      autoClosing(InputStreamJsonSeqReader.open(file)) { reader =>
        assert(reader.iterator.map(_.value.as[A].orThrow).toList == List(
          A(1, "a"),
          A(2, "b"),
          A(3, "c")))
      }
    }

  if sys.props contains "test.speed" then
    val m = 3
    val n = 10000

    val event = X(1, 1112223334445556667L, true, "ddddddddddddddddddddd", "eeeeeeeeeeeeeeeeeee", "ffffffffffffffffffffffffff")

    "Speed test" - {
      "toJson" in:
        val stopwatch = new Stopwatch
        for _ <- 1 to 2 do
          for i <- 1 to n do
            val a: KeyedEvent[event.type] = i.toString <-: event
            Stamped(i, Timestamp.Epoch, i.toString <-: event: KeyedEvent[MyEvent]).asJson
          info("toJson: " + stopwatch.itemsPerSecondString(n, "documents"))

      "Uncompressed" - {
        addFileTests()//identity, identity)

      //"gzip" - {
      //  addFileTests(out => new GZIPOutputStream(out), in => new GZIPInputStream(in))
      //}
      }
    }

    def addFileTests(/*outputFilter: OutputStream => OutputStream, inputFilter: InputStream => InputStream*/): Unit =
      "OutputStreamJsonSeqWriter with flush at end" in:
        withTemporaryFile { file =>
          autoClosing(new OutputStreamJsonSeqWriter(new FileOutputStream(file.toFile))) { w =>
            val stopwatch = new Stopwatch
            for _ <- 1 to m do
              for i <- 1 to n do
                w.writeJson(Stamped(i, Timestamp.Epoch, i.toString <-: event: KeyedEvent[MyEvent]).asJson)
              w.flush()
              info("OutputStreamJsonSeqWriter: " + stopwatch.itemsPerSecondString(n, "events"))
          }
          info(s"${Files.size(file)} bytes,  ${Files.size(file) / (m * n)}/document")
        }

      "OutputStreamJsonSeqWriter with flush after every document" in:
        withTemporaryFile { file =>
          autoClosing(new OutputStreamJsonSeqWriter(new FileOutputStream(file.toFile))) { w =>
            val stopwatch = new Stopwatch
            for _ <- 1 to m do
              for i <- 1 to n do
                w.writeJson(Stamped(i, Timestamp.Epoch, i.toString <-: event: KeyedEvent[MyEvent]).asJson)
                w.flush()
              info("flush: " + stopwatch.itemsPerSecondString(n, "events"))
            info(s"${Files.size(file)} bytes,  ${Files.size(file) / (m * n)}/document")
          }

          for _ <- 1 to 5 do
          autoClosing(InputStreamJsonSeqReader.open(file)) { reader =>
            val iterator: Iterator[Json] = reader.iterator.map(_.value)
            for _ <- 1 to m do
              val stopwatch = new Stopwatch
              var dummy = 0
              for _ <- 1 to n do
                dummy += iterator.next().jsonObjectOrThrow.toMap.size
              info("read: " + stopwatch.itemsPerSecondString(n, "events"))
              assert(dummy == n * (3 + 6))  // To avoid loop optimiziation
            assert(!iterator.hasNext)
          }
        }

      "OutputStreamJsonSeqWriter with sync" in:
        withTemporaryFile { file =>
          autoClosing(new FileOutputStream(file.toFile)) { fileOut =>
            val out = fileOut
            val w = new OutputStreamJsonSeqWriter(out)
            val stopwatch = new Stopwatch
            for _ <- 1 to 2 do
              val n = 100
              for i <- 1 to n do
                w.writeJson(Stamped(i, Timestamp.Epoch, i.toString <-: event: KeyedEvent[MyEvent]).asJson)
                w.flush()
                fileOut.getFD.sync()
              info("sync: " + stopwatch.itemsPerSecondString(n, "events"))
          }
        }


object FileJsonSeqTest:
  private trait MyEvent extends Event.IsKeyBase[MyEvent]:
    val keyCompanion: MyEvent.type = MyEvent

  private object MyEvent extends Event.CompanionForKey[String, MyEvent]:
    implicit val implicitSelf: MyEvent.type = this
    implicit val jsonCodec: TypedJsonCodec[MyEvent] = TypedJsonCodec(
      Subtype[X])

  private case class X(a: Int, b: Long, c: Boolean, d: String, e: String, f: String) extends MyEvent
  private object X:
    implicit val jsonCodec: Codec.AsObject[X] = deriveCodec

  private implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[MyEvent] =
    KeyedEvent.typedJsonCodec(KeyedEventTypedJsonCodec.KeyedSubtype.singleEvent[X])

  private final case class A(number: Int, string: String)
  private object A:
    implicit val jsonCodec: Codec.AsObject[A] = deriveCodec

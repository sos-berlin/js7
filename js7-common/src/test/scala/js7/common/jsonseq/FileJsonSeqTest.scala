package js7.common.jsonseq

import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Json}
import java.io.FileOutputStream
import java.nio.file.Files
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.io.file.FileUtils.implicits.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{touchFile, withTemporaryFile}
import js7.base.test.Test
import js7.base.time.{Stopwatch, Timestamp}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.jsonseq.FileJsonSeqTest.*
import js7.data.event.{Event, KeyedEvent, KeyedEventTypedJsonCodec, Stamped}

/**
  * @author Joacim Zschimmer
  */
final class FileJsonSeqTest extends Test {

  "Empty file" in {
    withTemporaryFile { file =>
      touchFile(file)
      autoClosing(InputStreamJsonSeqReader.open(file)) { reader =>
        assert(reader.read().isEmpty)
      }
    }
  }

  "File with no JSON document is empty" in {
    withTemporaryFile { file =>
      autoClosing(new OutputStreamJsonSeqWriter(new FileOutputStream(file))) { _ => }
      assert(Files.size(file) == 0)
      autoClosing(InputStreamJsonSeqReader.open(file)) { reader =>
        assert(reader.read().isEmpty)
      }
    }
  }

  "Some JSON document" in {
    withTemporaryFile { file =>
      autoClosing(new OutputStreamJsonSeqWriter(new FileOutputStream(file))) { w =>
        w.writeJson(A(1, "a").asJson)
        w.writeJson(A(2, "b").asJson)
        w.writeJson(A(3, "c").asJson)
        w.flush()
      }
      //assert(file.contentString startsWith Ascii.RS.toChar.toString)
      assert(file.contentString endsWith "\n")
      autoClosing(InputStreamJsonSeqReader.open(file)) { reader =>
        assert(reader.iterator.map(_.value.as[A].orThrow).toList == List(
          A(1, "a"),
          A(2, "b"),
          A(3, "c")))
      }
    }
  }

  if (sys.props contains "test.speed") {
    val m = 3
    val n = 10000
    case class X(a: Int, b: Long, c: Boolean, d: String, e: String, f: String) extends Event {
      type Key = String
    }
    implicit val XJsonCodec: Codec.AsObject[X] = deriveCodec

    implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[X] =
      KeyedEvent.typedJsonCodec(KeyedEventTypedJsonCodec.KeyedSubtype.singleEvent[X])

    val x = X(1, 1112223334445556667L, true, "ddddddddddddddddddddd", "eeeeeeeeeeeeeeeeeee", "ffffffffffffffffffffffffff")

    "Speed test" - {
      "toJson" in {
        val stopwatch = new Stopwatch
        for (_ <- 1 to 2) {
          for (i <- 1 to n) {
            Stamped(i, Timestamp.Epoch, KeyedEvent(x)(i.toString)).asJson
          }
          info("toJson: " + stopwatch.itemsPerSecondString(n, "documents"))
        }
      }

      "Uncompressed" - {
        addFileTests()//identity, identity)
      }

      //"gzip" - {
      //  addFileTests(out => new GZIPOutputStream(out), in => new GZIPInputStream(in))
      //}
    }

    def addFileTests(/*outputFilter: OutputStream => OutputStream, inputFilter: InputStream => InputStream*/): Unit = {
      "OutputStreamJsonSeqWriter with flush at end" in {
        withTemporaryFile { file =>
          autoClosing(new OutputStreamJsonSeqWriter(new FileOutputStream(file.toFile))) { w =>
            val stopwatch = new Stopwatch
            for (_ <- 1 to m) {
              for (i <- 1 to n) {
                w.writeJson(Stamped(i, Timestamp.Epoch, KeyedEvent(x)(i.toString)).asJson)
              }
              w.flush()
              info("OutputStreamJsonSeqWriter: " + stopwatch.itemsPerSecondString(n, "events"))
            }
          }
          info(s"${Files.size(file)} bytes,  ${Files.size(file) / (m * n)}/document")
        }
      }

      "OutputStreamJsonSeqWriter with flush after every document" in {
        withTemporaryFile { file =>
          autoClosing(new OutputStreamJsonSeqWriter(new FileOutputStream(file.toFile))) { w =>
            val stopwatch = new Stopwatch
            for (_ <- 1 to m) {
              for (i <- 1 to n) {
                w.writeJson(Stamped(i, Timestamp.Epoch, KeyedEvent(x)(i.toString)).asJson)
                w.flush()
              }
              info("flush: " + stopwatch.itemsPerSecondString(n, "events"))
            }
            info(s"${Files.size(file)} bytes,  ${Files.size(file) / (m * n)}/document")
          }

          for (_ <- 1 to 5)
          autoClosing(InputStreamJsonSeqReader.open(file)) { reader =>
            val iterator: Iterator[Json] = reader.iterator.map(_.value)
            for (_ <- 1 to m) {
              val stopwatch = new Stopwatch
              var dummy = 0
              for (_ <- 1 to n) {
                dummy += iterator.next().jsonObjectOrThrow.toMap.size
              }
              info("read: " + stopwatch.itemsPerSecondString(n, "events"))
              assert(dummy == n * (3 + 6))  // To avoid loop optimiziation
            }
            assert(!iterator.hasNext)
          }
        }
      }

      "OutputStreamJsonSeqWriter with sync" in {
        withTemporaryFile { file =>
          autoClosing(new FileOutputStream(file.toFile)) { fileOut =>
            val out = fileOut
            val w = new OutputStreamJsonSeqWriter(out)
            val stopwatch = new Stopwatch
            for (_ <- 1 to 2) {
              val n = 100
              for (i <- 1 to n) {
                w.writeJson(Stamped(i, Timestamp.Epoch, KeyedEvent(x)(i.toString)).asJson)
                w.flush()
                fileOut.getFD.sync()
              }
              info("sync: " + stopwatch.itemsPerSecondString(n, "events"))
            }
          }
        }
      }
    }
  }
}

object FileJsonSeqTest {
  final case class A(number: Int, string: String)
  object A {
    implicit val jsonCodec: Codec.AsObject[A] = deriveCodec
  }
}

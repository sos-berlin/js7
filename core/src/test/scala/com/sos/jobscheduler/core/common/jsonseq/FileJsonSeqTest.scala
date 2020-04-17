package com.sos.jobscheduler.core.common.jsonseq

import com.google.common.io.MoreFiles.touch
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowableEither
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryFile
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.core.common.jsonseq.FileJsonSeqTest._
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, KeyedEventTypedJsonCodec, Stamped}
import io.circe.Json
import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import java.io.FileOutputStream
import java.nio.file.Files
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileJsonSeqTest extends FreeSpec {

  "Empty file" in {
    withTemporaryFile { file =>
      touch(file)
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
        assert((reader.iterator map { _.value.as[A].orThrow }).toList == List(
          A(1, "a"),
          A(2, "b"),
          A(3, "c")))
      }
    }
  }

  if (sys.props contains "test.speed") {
    val m = 3
    val n = 10000
    @JsonCodec
    case class X(a: Int, b: Long, c: Boolean, d: String, e: String, f: String) extends Event {
      type Key = String
    }
    implicit val keyedEventJsonCodec = KeyedEvent.typedJsonCodec[X](KeyedEventTypedJsonCodec.KeyedSubtype.singleEvent[X])

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
            val iterator: Iterator[Json] = reader.iterator map (_.value)
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
  @JsonCodec
  final case class A(number: Int, string: String)
}

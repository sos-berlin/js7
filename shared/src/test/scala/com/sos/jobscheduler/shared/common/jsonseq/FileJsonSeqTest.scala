package com.sos.jobscheduler.shared.common.jsonseq

import com.google.common.base.Ascii
import com.google.common.io.Files.touch
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryFile
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Snapshot}
import com.sos.jobscheduler.shared.common.jsonseq.FileJsonSeqTest._
import java.io.{FileInputStream, FileOutputStream, InputStream, OutputStream}
import java.nio.file.Files
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import org.scalatest.FreeSpec
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class FileJsonSeqTest extends FreeSpec {

  private implicit val aJsonFormat = jsonFormat2(A)

  "Empty file" in {
    withTemporaryFile { file ⇒
      touch(file)
      autoClosing(new FileInputStream(file)) { in ⇒
        val iterator = new InputStreamJsonSeqIterator(in)
        assert(!iterator.hasNext)
      }
    }
  }

  "File with no JSON document is empty" in {
    withTemporaryFile { file ⇒
      autoClosing(new OutputStreamJsonSeqWriter(new FileOutputStream(file))) { _ ⇒ }
      assert(Files.size(file) == 0)
      autoClosing(new FileInputStream(file)) { in ⇒
        val iterator = new InputStreamJsonSeqIterator(in)
        assert(!iterator.hasNext)
      }
    }
  }

  "Some JSON document" in {
    withTemporaryFile { file ⇒
      autoClosing(new OutputStreamJsonSeqWriter(new FileOutputStream(file))) { w ⇒
        w.writeJson(A(1, "a").toJson)
        w.writeJson(A(2, "b").toJson)
        w.writeJson(A(3, "c").toJson)
        w.flush()
      }
      assert(file.contentString startsWith Ascii.RS.toChar.toString)
      assert(file.contentString endsWith "\n")
      autoClosing(new FileInputStream(file)) { in ⇒
        val iterator = new InputStreamJsonSeqIterator(in)
        assert((iterator map { _.convertTo[A] }).toList == List(
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
    implicit val xJsonFormat = jsonFormat6(X)
    implicit val keyedEventJsonFormat = KeyedEvent.typedJsonFormat[X](KeyedSubtype[X])

    val x = X(1, 1112223334445556667L, true, "ddddddddddddddddddddd", "eeeeeeeeeeeeeeeeeee", "ffffffffffffffffffffffffff")

    "Speed test" - {
      "toJson" in {
        val stopwatch = new Stopwatch
        for (_ ← 1 to 2) {
          for (i ← 1 to n) {
            Snapshot(i, KeyedEvent(x)(i.toString)).toJson
          }
          info("toJson: " + stopwatch.itemsPerSecondString(n, "document"))
        }
      }

      "Uncompressed" - {
        addFileTests(identity, identity)
      }

      "gzip" - {
        addFileTests(out ⇒ new GZIPOutputStream(out), in ⇒ new GZIPInputStream(in))
      }
    }

    def addFileTests(outputFilter: OutputStream ⇒ OutputStream, inputFilter: InputStream ⇒ InputStream): Unit = {
      "OutputStreamJsonSeqWriter with flush at end" in {
        withTemporaryFile { file ⇒
          autoClosing(new OutputStreamJsonSeqWriter(outputFilter(new FileOutputStream(file)))) { w ⇒
            val stopwatch = new Stopwatch
            for (_ ← 1 to m) {
              for (i ← 1 to n) {
                w.writeJson(Snapshot(i, KeyedEvent(x)(i.toString)).toJson)
              }
              w.flush()
              info("OutputStreamJsonSeqWriter: " + stopwatch.itemsPerSecondString(n, "event"))
            }
          }
          info(Files.size(file) + " bytes,  " + Files.size(file) / (m*n) + "/document")
        }
      }

      "OutputStreamJsonSeqWriter with flush after every document" in {
        withTemporaryFile { file ⇒
          autoClosing(new OutputStreamJsonSeqWriter(outputFilter(new FileOutputStream(file)))) { w ⇒
            val stopwatch = new Stopwatch
            for (_ ← 1 to m) {
              for (i ← 1 to n) {
                w.writeJson(Snapshot(i, KeyedEvent(x)(i.toString)).toJson)
                w.flush()
              }
              info("flush: " + stopwatch.itemsPerSecondString(n, "event"))
            }
            info(Files.size(file) + " bytes,  " + Files.size(file) / (m*n) + "/document")
          }

          for (_ ← 1 to 5)
          autoClosing(inputFilter(new FileInputStream(file))) { in ⇒
            val iterator = new InputStreamJsonSeqIterator(in)
            for (_ ← 1 to m) {
              val stopwatch = new Stopwatch
              var dummy = 0
              for (_ ← 1 to n) {
                dummy += iterator.next().asJsObject.fields.size
              }
              info("read: " + stopwatch.itemsPerSecondString(n, "event"))
              assert(dummy == n * (3 + 6))  // To avoid loop optimiziation
            }
            assert(!iterator.hasNext)
          }
        }
      }

      "OutputStreamJsonSeqWriter with sync" in {
        withTemporaryFile { file ⇒
          autoClosing(new FileOutputStream(file)) { fileOut ⇒
            val out = outputFilter(fileOut)
            val w = new OutputStreamJsonSeqWriter(out)
            val stopwatch = new Stopwatch
            for (_ ← 1 to 2) {
              val n = 100
              for (i ← 1 to n) {
                w.writeJson(Snapshot(i, KeyedEvent(x)(i.toString)).toJson)
                w.flush()
                fileOut.getFD.sync()
              }
              info("sync: " + stopwatch.itemsPerSecondString(n, "event"))
            }
          }
        }
      }
    }
  }
}

object FileJsonSeqTest {
  private case class A(number: Int, string: String)
}

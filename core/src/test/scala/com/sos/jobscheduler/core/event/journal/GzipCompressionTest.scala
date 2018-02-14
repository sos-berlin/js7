package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.event.journal.GzipCompression.isGzipCompressed
import com.sos.jobscheduler.core.event.journal.GzipCompressionTest._
import java.io.{BufferedReader, FileInputStream, FileOutputStream, InputStreamReader, OutputStreamWriter}
import java.nio.file.Files
import org.scalatest.Assertions._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GzipCompressionTest extends FreeSpec {

  "GzipCompression decompresses" in {
    autoClosing(new Tester) { tester ⇒
      tester.write(gzip = true)
      assert(isGzipCompressed(tester.file))
      tester.testRead()
    }
  }

  "GzipCompression reads uncompressed data" in {
    autoClosing(new Tester) { tester ⇒
      tester.write(gzip = false)
      assert(!isGzipCompressed(tester.file))
      tester.testRead()
    }
  }
}

object GzipCompressionTest {
  private val string ="""{"json": true}"""

  private class Tester extends AutoCloseable {
    val file = Files.createTempFile("test-", ".tmp")

    def close() = file.delete

    def write(gzip: Boolean) = {
      val conversion = new GzipCompression {
        override def compressWithGzip = gzip
      }
      autoClosing(new FileOutputStream(file)) { out ⇒
        val writer = new OutputStreamWriter(conversion.convertOutputStream(out))
        writer.write(string)
        writer.close()
      }
    }

    def testRead() = {
      val conversion = new GzipCompression {}
      autoClosing(new FileInputStream(file)) { in ⇒
        assert(new BufferedReader(new InputStreamReader(conversion.convertInputStream(in, file))).readLine() == string)
      }
    }
  }
}

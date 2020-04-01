package com.sos.jobscheduler.common.files

import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryFile
import org.scalatest.FreeSpec
import scala.util.Random
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
final class ByteVectorReaderTest extends FreeSpec
{
  "ByteVectorReader" in {
    withTemporaryFile("ByteVectorReaderTest", ".tmp") { file =>
      val bytes = ByteVector(Random.alphanumeric.map(_.toByte).take(3 * ByteVectorReader.ChunkSize - 7).toVector)
      file := bytes
      var read = ByteVector.empty
      autoClosing(new ByteVectorReader(file)) { reader =>
        var eof = false
        while (!eof) {
          val chunk = reader.read()
          if (chunk.isEmpty)
            eof = true
          else
            read ++= chunk
        }
      }
      assert(read == bytes)
    }
  }
}

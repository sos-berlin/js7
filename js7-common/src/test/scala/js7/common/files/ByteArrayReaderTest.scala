package js7.common.files

import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.withTemporaryFile
import js7.base.utils.AutoClosing.autoClosing
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class ByteArrayReaderTest extends AnyFreeSpec
{
  "ByteArrayReader" in {
    withTemporaryFile("ByteArrayReaderTest", ".tmp") { file =>
      val bytes = ByteArray.fromSeq(Random.alphanumeric.map(_.toByte).take(3 * ByteArrayReader.ChunkSize - 7))
      file := bytes
      var read = ByteArray.empty
      autoClosing(new ByteArrayReader(file)) { reader =>
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

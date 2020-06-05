package js7.core.common.jsonseq

import akka.util.ByteString
import com.google.common.base.Ascii
import io.circe.Json
import java.io.{BufferedOutputStream, OutputStream}
import js7.base.circeutils.CirceUtils._
import js7.base.utils.Assertions.assertThat
import js7.core.common.jsonseq.OutputStreamJsonSeqWriter._
import org.jetbrains.annotations.TestOnly

/**
  * MIME media type application/json-seq, RFC 7464 "JavaScript Object Notation (JSON) Text Sequences".
  *
  * @author Joacim Zschimmer
  * @see https://tools.ietf.org/html/rfc7464
  */
final class OutputStreamJsonSeqWriter(out: OutputStream, withRS: Boolean = false)
extends AutoCloseable
{
  private val extraLength = if (withRS) 2 else 1
  private var _written = 0L

  private val buffered = out match {
    case o: BufferedOutputStream => o
    case o => new BufferedOutputStream(o)
  }
  private var array: Array[Byte] = null

  def close() = buffered.close()

  @TestOnly
  private[jsonseq] def writeJson(json: Json): Unit =
    writeJson(ByteString.fromString(json.compactPrint))

  def writeJson(byteString: ByteString): Unit = {
    if (withRS) buffered.write(Ascii.RS)
    val length = byteString.length
    if (array == null || array.length < length + 1) {
      array = new Array[Byte](length + 1 + Reserve)
    }
    byteString.copyToArray(array)
    array(length) = '\n'
    assertThat(array.indexOf('\n') == length, "OutputStreamJsonSeqWriter: JSON contains a forbidden LF")
    buffered.write(array, 0, length + 1)
    _written += length + extraLength
  }

  def flush() = buffered.flush()

  def bytesWritten = _written
}

object OutputStreamJsonSeqWriter {
  private val Reserve = 4000
}

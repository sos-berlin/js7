package js7.common.jsonseq

import io.circe.Json
import java.io.{BufferedOutputStream, OutputStream}
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.utils.Ascii.RS
import js7.base.utils.Assertions.assertThat
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
  private val extraLength = if withRS then 2 else 1
  private var _written = 0L

  private val buffered = out match {
    case o: BufferedOutputStream => o
    case o => new BufferedOutputStream(o)
  }

  def close() = buffered.close()

  @TestOnly
  private[jsonseq] def writeJson(json: Json): Unit =
    writeJson(json.toByteArray)

  def writeJson(byteArray: ByteArray): Unit = {
    if withRS then buffered.write(RS)
    assertThat(byteArray.indexOf('\n') == -1, "OutputStreamJsonSeqWriter: JSON contains a forbidden LF")
    buffered.write(byteArray.unsafeArray)
    buffered.write('\n')
    _written += byteArray.length + extraLength
  }

  def flush() = buffered.flush()

  def bytesWritten = _written
}

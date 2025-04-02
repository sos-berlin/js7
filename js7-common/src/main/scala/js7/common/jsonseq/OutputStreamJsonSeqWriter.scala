package js7.common.jsonseq

import io.circe.Json
import java.io.{BufferedOutputStream, OutputStream}
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.utils.Assertions.assertThat
import org.jetbrains.annotations.TestOnly

/**
  * MIME media type application/json-seq, RFC 7464 "JavaScript Object Notation (JSON) Text Sequences".
  *
  * @author Joacim Zschimmer
  * @see https://tools.ietf.org/html/rfc7464
  */
final class OutputStreamJsonSeqWriter(out: OutputStream)
extends AutoCloseable:

  private var _bytesWritten = 0L

  private val buffered = out match
    case o: BufferedOutputStream => o
    case o => new BufferedOutputStream(o)

  def close(): Unit =
    buffered.close()

  @TestOnly
  private[jsonseq] def writeJson(json: Json): Unit =
    writeJson(json.toByteArray)

  def writeJson(byteArray: ByteArray): Unit =
    assertThat(byteArray.indexOf('\n') == -1, "OutputStreamJsonSeqWriter: JSON contains a forbidden LF")
    buffered.write(byteArray.unsafeArray)
    buffered.write('\n')
    _bytesWritten += byteArray.length + 1

  def flush(): Unit =
    buffered.flush()

  def bytesWritten: Long =
    _bytesWritten

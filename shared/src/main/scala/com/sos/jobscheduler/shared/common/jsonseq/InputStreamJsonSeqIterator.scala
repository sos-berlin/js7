package com.sos.jobscheduler.shared.common.jsonseq

import com.google.common.base.Ascii
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import io.circe.Json
import java.io.{BufferedReader, EOFException, InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.AbstractIterator
import scala.util.Try

/**
  * MIME media type application/json-seq, RFC 7464 "JavaScript Object Notation (JSON) Text Sequences".
  * <p>
  *   This implementation depends on BufferedReader.readLine, which separates the input not only
  *   by LF but also by CR, CR/LF and EOF.
  * <p>
  *    Also, this class does not collapse consecutive RS.
  *
  * @author Joacim Zschimmer
  * @see https://tools.ietf.org/html/rfc7464
  */
final class InputStreamJsonSeqIterator(in: InputStream) extends AbstractIterator[Json] {

  private val reader = new BufferedReader(new InputStreamReader(in, UTF_8))

  private var lineNumber = 0
  private var nextJsValue: Try[Option[Json]] = readJsValue()

  def hasNext = nextJsValue.get.nonEmpty

  def next() = {
    val result = nextJsValue.get getOrElse { throw new NoSuchElementException("InputStreamJsonSeqIterator") }
    nextJsValue = readJsValue()
    result
  }

  private def readJsValue(): Try[Option[Json]] = Try {
    lineNumber += 1
    reader.read() match {
      case Ascii.RS ⇒
        val line = reader.readLine()
        if (line == null) throw new EOFException("Unexpected end of file in the middle of an entry: RS but no LF")
        // BufferedReader#readLine separates the lines also by CR, CR/LF and EOF. RFC 7464 requires strict LF !!!
        Some(line.parseJson)
      case -1 ⇒
        None  // EOF
      case o ⇒
        throwCorrupted(s"Invalid character \\u${Integer.toHexString(o)}")
    }
  }

  private def throwCorrupted(extra: String) = sys.error(s"JSON sequence is corrupted at line $lineNumber. $extra")
}

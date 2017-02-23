package com.sos.scheduler.engine.shared.common.jsonseq

import com.google.common.base.Ascii
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.shared.common.jsonseq.InputStreamJsonSeqIterator._
import java.io.{BufferedReader, EOFException, InputStream, InputStreamReader}
import scala.collection.AbstractIterator
import scala.io
import spray.json._

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
final class InputStreamJsonSeqIterator(in: InputStream) extends AbstractIterator[JsValue] {

  private val reader = new BufferedReader(new InputStreamReader(in))

  private var lineNumber = 0
  private var nextLineOption: Option[String] = readJsonString()

  def hasNext = nextLineOption.nonEmpty

  def next() = {
    val result = nextLineOption getOrElse { throw new NoSuchElementException("InputStreamJsonSeqIterator") }
    nextLineOption = readJsonString()
    result.parseJson
  }

  private def readJsonString(): Option[String] = {
    lineNumber += 1
    try reader.read() match {
      case Ascii.RS ⇒
        val line = Option(reader.readLine()) getOrElse throwCorrupted()
        // BufferedReader#readLine separates the lines also by CR, CR/LF and EOF. RFC 7464 requires strict LF !!!
        //val valid = line.nonEmpty && line.last == '\n'
        //if (!valid) throwCorrupted()
        Some(line)
      case -1 ⇒
        None  // EOF
      case _ ⇒
        throwCorrupted()
    }
    //catch {
    //  TODO Bei einem Fehler können wir das Journal nicht fortschreiben. Wir müssen es neu aufbauen.
    //  case _: EOFException ⇒
    //    logger.warn("Journal is truncated. Restart from improper termination is assumed, using the events read so far.")  // TODO Bestätigen lassen?
    //    None
    //  case t: java.util.zip.ZipException ⇒
    //    logger.warn(s"Journal is corrupt . Restart from improper termination is assumed, using the events read so far. $t", t)  // TODO Bestätigen lassen?
    //    None
    //}
  }

  private def throwCorrupted() = sys.error(s"JSON sequence is corrupted at line $lineNumber")
}

object InputStreamJsonSeqIterator {
  private val logger = Logger(getClass)
}

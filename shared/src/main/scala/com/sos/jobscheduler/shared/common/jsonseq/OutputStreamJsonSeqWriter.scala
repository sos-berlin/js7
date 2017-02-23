package com.sos.scheduler.engine.shared.common.jsonseq

import com.google.common.base.Ascii
import java.io.{BufferedOutputStream, OutputStream, OutputStreamWriter}
import spray.json.JsValue

/**
  * MIME media type application/json-seq, RFC 7464 "JavaScript Object Notation (JSON) Text Sequences".
  *
  * @author Joacim Zschimmer
  * @see https://tools.ietf.org/html/rfc7464
  */
final class OutputStreamJsonSeqWriter(out: OutputStream) extends AutoCloseable {

  private val writer = new OutputStreamWriter(new BufferedOutputStream(out))

  def close(): Unit =
    writer.close()

  def writeJson(jsValue: JsValue): Unit = {
    writer.write(Ascii.RS)
    writer.write(jsValue.compactPrint)
    writer.write('\n')
  }

  def flush(): Unit =
    writer.flush()
}

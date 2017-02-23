package com.sos.scheduler.engine.shared.event.journal

import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.shared.common.jsonseq.OutputStreamJsonSeqWriter
import com.sos.scheduler.engine.shared.event.journal.FileJsonWriter._
import java.io.{FileOutputStream, OutputStream}
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicBoolean
import spray.json.{JsObject, JsValue}

/**
  * @author Joacim Zschimmer
  */
final class FileJsonWriter(
  header: JsObject,
  convertOutput: OutputStream ⇒ OutputStream,
  val file: Path,
  val syncOnFlush: Boolean,
  append: Boolean = false)
extends AutoCloseable {

  private val out = new FileOutputStream(file, append)
  private val convertingOut = convertOutput(out)
  private val writer = new OutputStreamJsonSeqWriter(convertingOut)
  private val closed = new AtomicBoolean
  private var flushed = false

  if (!append) {
    writer.writeJson(header)
  }

  def writeJson(jsValue: JsValue): Unit = {
    flushed = false
    writer.writeJson(jsValue)
  }

  def close() = {
    if (closed.compareAndSet(false, true)) {
      flush()
      writer.close()
      convertingOut.close()
      out.close()
      logger.debug("Closed")
    }
  }

  /**
    * Flushes and optionally syncs the previously written items.
    *
    * @return Wether something has been flushed
    */
  def flush(): Unit = {
    !flushed && {
      writer.flush()
      convertingOut.flush()
      out.flush()
      if (syncOnFlush) {
        out.getFD.sync()
      }
      flushed = true
      true
    }
  }
}

object FileJsonWriter {
  private val logger = Logger(getClass)
}

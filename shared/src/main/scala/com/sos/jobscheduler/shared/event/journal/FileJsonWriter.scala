package com.sos.jobscheduler.shared.event.journal

import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.shared.common.jsonseq.OutputStreamJsonSeqWriter
import com.sos.jobscheduler.shared.event.journal.FileJsonWriter._
import java.io.{BufferedOutputStream, FileOutputStream, OutputStream}
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicBoolean
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class FileJsonWriter(
  header: JsonJournalHeader,
  convertOutput: OutputStream ⇒ OutputStream,
  val file: Path,
  val syncOnFlush: Boolean,
  append: Boolean = false)
extends AutoCloseable {

  private val out = new FileOutputStream(file, append)
  private val bufferedOut = new BufferedOutputStream(out)
  private val convertingOut = convertOutput(bufferedOut)
  private val writer = new OutputStreamJsonSeqWriter(convertingOut)
  private val closed = new AtomicBoolean
  private var flushed = false

  if (!append) {
    writer.writeJson(ByteString.fromString(header.toJson.compactPrint))
    flush()
  }

  def writeJson(json: ByteString): Unit = {
    flushed = false
    writer.writeJson(json)
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

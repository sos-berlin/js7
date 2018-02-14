package com.sos.jobscheduler.core.event.journal

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.common.jsonseq.OutputStreamJsonSeqWriter
import io.circe.syntax.EncoderOps
import java.io.{BufferedOutputStream, FileOutputStream, OutputStream}
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicBoolean

/**
  * @author Joacim Zschimmer
  */
final class FileJsonWriter(
  header: JournalHeader,
  convertOutput: OutputStream ⇒ OutputStream,
  val file: Path,
  append: Boolean = false)
extends AutoCloseable {

  private val out = new FileOutputStream(file, append)
  private val bufferedOut = new BufferedOutputStream(out)
  private val convertingOut = convertOutput(bufferedOut)
  private val writer = new OutputStreamJsonSeqWriter(convertingOut)
  private val closed = new AtomicBoolean
  private var flushed = false
  private var synced = false

  if (!append) {
    writer.writeJson(ByteString.fromString(header.asJson.compactPrint))
    flush()
  }

  def writeJson(json: ByteString): Unit = {
    flushed = false
    synced = false
    writer.writeJson(json)
  }

  def close() = {
    if (closed.compareAndSet(false, true)) {
      flush()
      writer.close()
      convertingOut.close()
      out.close()
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
      flushed = true
      true
    }
  }

  def sync(): Unit = {
    out.getFD.sync()
    synced = true
  }

  def isFlushed = flushed

  def isSynced = synced
}

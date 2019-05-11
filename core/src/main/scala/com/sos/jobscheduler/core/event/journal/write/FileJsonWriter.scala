package com.sos.jobscheduler.core.event.journal.write

import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.common.jsonseq.OutputStreamJsonSeqWriter
import java.io.{BufferedOutputStream, FileOutputStream}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class FileJsonWriter(
  val file: Path,
  append: Boolean = false,
  simulateSync: Option[FiniteDuration] = None)
extends AutoCloseable {

  private val out = wrapException { new FileOutputStream(file, append) }
  private val bufferedOut = new BufferedOutputStream(out)
  private val writer = new OutputStreamJsonSeqWriter(bufferedOut)
  private val closed = new AtomicBoolean
  private var flushed = false
  private var synced = false
  private val initialPosition = Files.size(file)

  def close() =
    wrapException {
      if (!closed.getAndSet(true)) {
        flush()
        writer.close()
        bufferedOut.close()
        out.close()
      }
    }

  def write(byteString: ByteString): Unit =
    wrapException {
      flushed = false
      synced = false
      writer.writeJson(byteString)
    }

  def sync(): Unit =
    wrapException {
      if (!synced) {
        flush()
        simulateSync match {
          case Some(duration) => Thread.sleep(duration.toMillis)
          case None => out.getFD.sync()
        }
        synced = true
      }
    }

  def flush(): Unit =
    wrapException {
      if (!flushed) {
        writer.flush()
        bufferedOut.flush()
        flushed = true
        synced = false
      }
    }

  def isFlushed = flushed

  def isSynced = synced

  def fileLength = initialPosition + bytesWritten

  def bytesWritten  = writer.bytesWritten

  protected def wrapException[A](body: => A): A =
    try body
    catch { case NonFatal(t) if t == null || t.getMessage == null || !t.getMessage.contains(file.toAbsolutePath.toString) =>
      throw new RuntimeException(s"Error while writing file '$file': $t", t)
    }
}

package js7.journal.write

import java.io.{BufferedOutputStream, FileOutputStream}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicBoolean
import js7.base.data.ByteArray
import js7.common.jsonseq.OutputStreamJsonSeqWriter
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

  private val out = wrapException { new FileOutputStream(file.toFile, append) }
  private val bufferedOut = new BufferedOutputStream(out)
  private val writer = new OutputStreamJsonSeqWriter(bufferedOut)
  private val closed = new AtomicBoolean
  private var flushed = false
  private var synced = false
  private val initialPosition = Files.size(file)

  def close() =
    wrapException {
      if !closed.getAndSet(true) then {
        flush()
        writer.close()
        bufferedOut.close()
        out.close()
      }
    }

  def write(byteArray: ByteArray): Unit =
    wrapException {
      flushed = false
      synced = false
      writer.writeJson(byteArray)
    }

  def sync(): Unit =
    wrapException {
      if !synced then {
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
      if !flushed then {
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

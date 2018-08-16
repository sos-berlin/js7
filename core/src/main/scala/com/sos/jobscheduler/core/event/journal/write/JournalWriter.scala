package com.sos.jobscheduler.core.event.journal.write

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.ByteUnits.toMB
import com.sos.jobscheduler.core.event.journal.data.JournalHeader
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.data.event.Event
import java.nio.file.{Files, Path}
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private[journal] abstract class JournalWriter[E <: Event](append: Boolean)
extends AutoCloseable {

  def file: Path
  protected def observer: Option[JournalingObserver]
  protected def simulateSync: Option[FiniteDuration]

  protected val logger = Logger.withPrefix[JournalWriter[E]](file.getFileName.toString)

  if (!append && Files.exists(file)) sys.error(s"JournalWriter: Not expecting existent files '$file'")
  if (append && !Files.exists(file)) sys.error(s"JournalWriter: Missing files '$file'")

  protected val jsonWriter = new FileJsonWriter(JournalHeader.Singleton, file, append = append, simulateSync = simulateSync)
  protected val statistics = new StatisticsCounter

  def close() = jsonWriter.close()

  protected def bytesWrittenString: String =
    try toMB(Files.size(file)) catch { case NonFatal(t) ⇒ t.toString }

  def flush(sync: Boolean): Unit = {
    if (!jsonWriter.isFlushed) {
      statistics.beforeFlush()
      jsonWriter.flush()
      statistics.afterFlush()
    }
    if (sync && !isSynced) {
      statistics.beforeSync()
      jsonWriter.sync()
      statistics.afterSync()
    }
  }

  def isFlushed = jsonWriter.isFlushed

  def isSynced = jsonWriter.isSynced
}

private[journal] object JournalWriter {
  final class SerializationException(cause: Throwable) extends RuntimeException("JSON serialization error", cause)
}

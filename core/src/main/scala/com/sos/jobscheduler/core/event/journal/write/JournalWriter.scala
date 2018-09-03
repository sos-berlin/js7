package com.sos.jobscheduler.core.event.journal.write

import com.sos.jobscheduler.common.utils.ByteUnits.toMB
import com.sos.jobscheduler.core.event.journal.data.JournalHeader
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.data.event.{Event, EventId}
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
  protected val after: EventId
  protected val statistics: StatisticsCounter

  if (!append && Files.exists(file)) sys.error(s"JournalWriter: Not expecting existent file '$file'")
  if (append && !Files.exists(file)) sys.error(s"JournalWriter: Missing file '$file'")

  protected final val jsonWriter = new FileJsonWriter(JournalHeader(eventId = after), file, append = append, simulateSync = simulateSync)

  def close() = jsonWriter.close()

  protected final def fileSizeString: String =
    try toMB(Files.size(file)) catch { case NonFatal(t) ⇒ t.toString }

  protected def flush(sync: Boolean): Unit = {
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

  final def isFlushed = jsonWriter.isFlushed

  final def isSynced = jsonWriter.isSynced
}

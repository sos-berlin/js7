package js7.journal

import cats.effect.SyncIO
import java.nio.file.Files
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.streamJournalFiles
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.util.control.NonFatal

sealed trait FileJournalMXBean:
  this: FileJournalMXBean.Bean =>

  def getFileSize: Long =
    fileSize

  def getAllFileSize: Long =
    // OPTIMISE Ask JournalEventWatch.fileEventIdToHistoric, but this is incomplete, because
    // unused files are evicted (maybe heap consumption doesn't matter?)
    journalLocation.fold(0L): loc =>
      streamJournalFiles[SyncIO](loc.fileBase).map: journalFile =>
        try Files.size(journalFile.file)
        catch case NonFatal(e) => 0L
      .foldMonoid // sum
      .compile.last.run().get

  def getEventTotal: Long =
    eventTotal.get

  def getPersistTotal: Long =
    persistTotal.get

  def getPersistSecondsTotal: Double =
    persistNanos.get / 1_000_000_000.0

  def getCommitTotal: Long =
    commitTotal.get

  def getFlushTotal: Long =
    flushTotal.get

  def getEventCalcSecondsTotal: Double =
    eventCalcNanos.get / 1_000_000_000.0

  def getJsonWriteSecondsTotal: Double =
    jsonWriteNanos.get / 1_000_000_000.0

  def getAckSecondsTotal: Double =
    ackNanos.get / 1_000_000_000.0

  def getOperatingSeconds: Double =
    (totalOperatingTimeUntilStart.get + sinceStart.elapsed).toDoubleSeconds

  def getActiveHeartbeatDelay: java.lang.Double | Null =
    activeHeartbeatDelay match
      case null => null
      case d: FiniteDuration => d.toDoubleSeconds


object FileJournalMXBean:

  final class Bean(protected val journalLocation: Option[JournalLocation])
  extends FileJournalMXBean:
    protected val sinceStart = Deadline.now
    private[journal] val totalOperatingTimeUntilStart = Atomic(ZeroDuration)

    var fileSize = 0L
    protected val eventTotal = Atomic(0L)
    private[journal] val persistTotal = Atomic(0L)
    private[journal] val persistNanos = Atomic(0L)
    private[journal] val commitTotal = Atomic(0L)
    private[journal] val flushTotal = Atomic(0L)
    private[journal] val eventCalcNanos = Atomic(0L)
    private[js7] val jsonWriteNanos = Atomic(0L)
    private[journal] val ackNanos = Atomic(0L)
    private[js7] var activeHeartbeatDelay: FiniteDuration | Null = null

    def addEventCount(n: Int): Unit =
      eventTotal += n


  object Bean:
    /** Use this when you don't need a FileJournalMXBean. */
    val dummy = Bean(None)

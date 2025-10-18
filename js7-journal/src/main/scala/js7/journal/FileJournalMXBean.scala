package js7.journal

import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import scala.concurrent.duration.{Deadline, FiniteDuration}

sealed trait FileJournalMXBean:
  this: FileJournalMXBean.Bean =>

  def getFileSize: Long =
    fileSize

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

  def getPassiveHeartbeatDelay: java.lang.Double | Null =
    passiveHeartbeatDelay match
      case null => null
      case d: FiniteDuration => d.toDoubleSeconds


object FileJournalMXBean:

  final class Bean extends FileJournalMXBean:
    protected val sinceStart = Deadline.now
    private[journal] val totalOperatingTimeUntilStart = Atomic(ZeroDuration)

    var fileSize = 0L
    protected val eventTotal = Atomic(0L)
    private[journal] val persistTotal = Atomic(0L)
    private[journal] val persistNanos = Atomic(0L)
    private[journal] val commitTotal = Atomic(0L)
    private[journal] val flushTotal = Atomic(0L)
    private[journal] val eventCalcNanos = Atomic(0L)
    private[journal] val jsonWriteNanos = Atomic(0L)
    private[journal] val ackNanos = Atomic(0L)
    private[js7] var passiveHeartbeatDelay: FiniteDuration | Null = null

    def addEventCount(n: Int): Unit =
      eventTotal += n

    private inline def ifPersisted[A <: AnyRef](inline a: A): A =
      if isUsed then a else null.asInstanceOf[A]

    private def isUsed = persistTotal.get > 0


  object Bean:
    /** Use this when you don't need a FileJournalMXBean. */
    val dummy = new Bean

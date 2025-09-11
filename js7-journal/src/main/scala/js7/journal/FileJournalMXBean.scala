package js7.journal

import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.Deadline

sealed trait FileJournalMXBean:
  def getFileSize: Long
  def getEventTotal: Long
  def getCommitTotal: Long
  def getPersistTotal: Long
  def getPersistSecondsTotal: Double
  def getFlushTotal: Long
  def getEventCalcSecondsTotal: Double
  def getJsonWriteSecondsTotal: Double
  def getAckSecondsTotal: Double
  def getOperatingSeconds: Double


object FileJournalMXBean:

  final class Bean extends FileJournalMXBean:
    private val sinceStart = Deadline.now
    private[journal] val totalOperatingTimeUntilStart = Atomic(ZeroDuration)

    var fileSize = 0L
    private val eventTotal = Atomic(0L)
    private[journal] val persistTotal = Atomic(0L)
    private[journal] val persistNanos = Atomic(0L)
    private[journal] val commitTotal = Atomic(0L)
    private[journal] val flushTotal = Atomic(0L)
    private[journal] val eventCalcNanos = Atomic(0L)
    private[journal] val jsonWriteNanos = Atomic(0L)
    private[journal] val ackNanos = Atomic(0L)

    def getFileSize: Long =
      fileSize

    def addEventCount(n: Int): Unit =
      eventTotal += n

    def getEventTotal: Long =
      eventTotal.get

    def getPersistTotal =
      persistTotal.get

    def getPersistSecondsTotal =
      persistNanos.get / 1_000_000_000.0

    def getCommitTotal: Long =
      commitTotal.get

    def getFlushTotal: Long =
      flushTotal.get

    def getEventCalcSecondsTotal =
      eventCalcNanos.get / 1_000_000_000.0

    def getJsonWriteSecondsTotal =
      jsonWriteNanos.get / 1_000_000_000.0

    def getAckSecondsTotal =
      ackNanos.get / 1_000_000_000.0

    def getOperatingSeconds: Double =
      (totalOperatingTimeUntilStart.get + sinceStart.elapsed).toDoubleSeconds


    private inline def ifPersisted[A <: AnyRef](inline a: A): A =
      if isUsed then a else null.asInstanceOf[A]

    private def isUsed = persistTotal.get > 0


  object Bean:
    /** Use this when you don't need a FileJournalMXBean. */
    val dummy = new Bean

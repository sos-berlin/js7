package js7.journal

import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*

sealed trait FileJournalMXBean:
  def getFileSize: Long
  def getEventTotal: Long
  def getCommitTotal: Long
  def getPersistTotal: Long
  def getFlushTotal: Long
  def getEventCalcSecondsTotal: Double
  def getJsonWriteSecondsTotal: Double
  def getAckSecondsTotal: Double


object FileJournalMXBean:

  final class Bean extends FileJournalMXBean:
    private[journal] var fileSize = 0L
    private val eventTotal = Atomic(0L)
    private[journal] val persistTotal = Atomic(0L)
    private[journal] val commitTotal = Atomic(0L)
    private[journal] val flushTotal = Atomic(0L)
    private[journal] val eventCalcNanos = Atomic(0L)
    private[journal] val jsonWriteNanos = Atomic(0L)
    private[journal] val ackNanos = Atomic(0L)

    def getFileSize: Long =
      fileSize

    private[journal] def addEventCount(n: Int): Unit =
      eventTotal += n

    def getEventTotal: Long =
      eventTotal.get

    def getPersistTotal: Long =
      persistTotal.get

    def getCommitTotal: Long =
      commitTotal.get

    def getFlushTotal: Long =
      flushTotal.get

    def getEventCalcSecondsTotal: Double =
      eventCalcNanos.get / 1_000_000_000.0

    override def getJsonWriteSecondsTotal: Double =
      jsonWriteNanos.get / 1_000_000_000.0

    def getAckSecondsTotal: Double =
      ackNanos.get / 1_000_000_000.0


  object Bean:
    /** Use this when you don't need a FileJournalMXBean. */
    val dummy = new Bean

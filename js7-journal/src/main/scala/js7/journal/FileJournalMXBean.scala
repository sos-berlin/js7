package js7.journal

import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*

sealed trait FileJournalMXBean:
  def getFileSize: Long
  def getEventTotal: Long
  def getFlushTotal: Long
  def getCommitTotal: Long


object FileJournalMXBean:

  final class Bean extends FileJournalMXBean:
    private[journal] var fileSize = 0L
    private val eventTotal = Atomic(0L)
    private[journal] val commitTotal = Atomic(0L)
    private[journal] val flushTotal = Atomic(0L)

    def getFileSize: Long =
      fileSize

    private[journal] def addEventCount(n: Int): Unit =
      eventTotal += n

    def getEventTotal: Long =
      eventTotal.get

    def getCommitTotal: Long =
      commitTotal.get

    def getFlushTotal: Long =
      flushTotal.get


  object Bean:
    /** Use this when you don't need a FileJournalMXBean. */
    val dummy = new Bean

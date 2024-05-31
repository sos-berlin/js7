package js7.journal.log

import com.typesafe.scalalogging.Logger
import java.util.Locale.ROOT
import js7.base.log.CorrelId
import js7.base.log.LoggingEscapeCodes.{isColorAllowed, resetColor}
import js7.base.time.ScalaTime.*
import js7.base.utils.Classes.superclassesOf
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, KeyedEvent, Stamped}
import js7.journal.log.JournalLogger.*
import scala.collection.{IndexedSeqView, mutable}
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now

private[journal] final class JournalLogger(
  syncOrFlushString: String,
  infoLogEvents: Set[String],
  suppressTiming: Boolean = false):

  private val syncOrFlushWidth = 6 max syncOrFlushString.length
  private val ackSyncOrFlushString = syncOrFlushString.toUpperCase(ROOT)

  private val infoLoggableEventClasses = new SubclassCache(infoLogEvents)
  private val sb = new StringBuilder
  private var suppressed = false

  //def logHeader(header: JournalHeader): Unit =
  //  logger.trace(
  //    f"  ${" " * syncOrFlushWidth}      * ${header.eventId}%16d $header")

  def logCommitted(persists: IndexedSeqView[Loggable], ack: Boolean = false): Unit =
    if !suppressed then logger.whenInfoEnabled:
      val committedAt = now
      val myPersists = dropEmptyPersists(persists)

      logger.whenTraceEnabled:
        logPersists(myPersists, committedAt)(traceLogPersist(ack))

      def isLoggable(stamped: Stamped[AnyKeyedEvent]) =
        val event = stamped.value.event
        infoLoggableEventClasses.contains(event.getClass) || !event.isSucceeded

      val loggablePersists = myPersists.filter(_.stampedSeq.exists(isLoggable))
      if loggablePersists.nonEmpty then
        logPersists(loggablePersists.toVector.view, committedAt, isLoggable)(infoLogPersist)

  def suppress(supressed: Boolean): Unit =
    this.suppressed = supressed

  private def dropEmptyPersists(persists: IndexedSeqView[Loggable]): IndexedSeqView[Loggable] =
    val dropLeft = persists.segmentLength(_.stampedSeq.isEmpty)
    val dropRight = persists.reverse.segmentLength(_.stampedSeq.isEmpty)
    persists.slice(dropLeft, persists.length - dropRight)

  private def logPersists(
    persists: IndexedSeqView[Loggable],
    committedAt: Deadline,
    isLoggable: Stamped[AnyKeyedEvent] => Boolean = _ => true)
    (body: (PersistFrame, Stamped[AnyKeyedEvent]) => Unit)
  : Unit =
    CorrelId.isolate { logCorrelId =>
      var index = 0
      for persist <- persists do
        logCorrelId := persist.correlId
        val stampedSeq = persist.stampedSeq.filter(isLoggable)
        val frame = PersistFrame(persist, stampedSeq.length, index, persists.length, committedAt)
        val stampedIterator = stampedSeq.iterator
        var hasNext = stampedIterator.hasNext
        while hasNext do
          val stamped = stampedIterator.next()
          hasNext = stampedIterator.hasNext
          frame.isLastEvent = !hasNext
          body(frame, stamped)
          frame.nr += 1
          frame.isFirstEvent = false
        index += 1
    }

  private def traceLogPersist(ack: Boolean)(frame: PersistFrame, stamped: Stamped[AnyKeyedEvent]): Unit =
    import frame.*
    sb.clear()
    sb.append(':')  // Allow simple filtering of log file: grep " - :" x.log
    //? sb.fillRight(5) { sb.append(nr) }
    sb.append(persistMarker)
    sb.fillRight(syncOrFlushWidth):
      if isLastEvent && persist.isLastOfFlushedOrSynced then
        sb.append(if ack then ackSyncOrFlushString else syncOrFlushString)
      else if isFirstEvent && persistIndex == 0 && persistCount >= 2 then
        sb.append(persistCount)  // Wrongly counts multiple isLastOfFlushedOrSynced (but only SnapshotTaken)
      else if nr == beforeLastEventNr && persistEventCount >= 10_000 then
        val micros = duration.toMicros
        if micros != 0 then
          val k = (1000.0 * persistEventCount / micros).toInt
          if k < 1000 then
            sb.append(k)
            sb.append("k/s")
          else
            sb.append(k / 1000)
            sb.append("M/s")

    if isLastEvent then
      sb.append(' ')
      sb.fillRight(6):
        if !suppressTiming && duration >= MinimumDuration then sb.append(duration.msPretty)
    else if nr == beforeLastEventNr && beforeLastEventNr > persist.eventNumber then
      sb.fillLeft(7) { sb.append(persistEventCount) }
    else
      sb.append("       ")

    sb.append(transactionMarker(true))
    sb.append(stamped.eventId)
    if stamped.value.key != NoKey then
      sb.append(' ')
      sb.append(stamped.value.key)
      sb.append(spaceArrow)

    locally:
      val event = stamped.value.event
      val eventString = event.toString.truncateWithEllipsis(200, firstLineOnly = true)
      if isColorAllowed && !event.isMinor then
        sb.append(" \u001b[39m\u001b[1m") // default color, bold
        val i = eventString.indexOfOrLength('(')
        sb.underlying.append(eventString, 0, i)
        sb.append("\u001b[0m") // all attributes off
        sb.underlying.append(eventString, i, eventString.length)
        sb.append(resetColor)
      else
        sb.append(' ')
        sb.append(eventString)

    logger.trace(sb.toString)

  private def infoLogPersist(frame: PersistFrame, stamped: Stamped[AnyKeyedEvent]): Unit =
    import frame.*
    import stamped.value.{event, key}
    sb.clear()
    sb.append("Event ")
    sb.append(transactionMarker(false))
    if key != NoKey then
      sb.append(key)
      sb.append(spaceArrowSpace)
    sb.append(event.toShortString)
    logger.info(sb.toString)


object JournalLogger:
  private val logger = Logger("js7.journal.Journal")
  private val MinimumDuration = 1.ms
  private val spaceArrow = " " + KeyedEvent.Arrow
  private val spaceArrowSpace = spaceArrow + " "

  private[journal] trait Loggable:
    def correlId: CorrelId
    def eventNumber: Long
    def stampedSeq: Seq[Stamped[AnyKeyedEvent]]
    def isTransaction: Boolean
    def since: Deadline
    def isLastOfFlushedOrSynced: Boolean

  final class SimpleLoggable(
    val correlId: CorrelId,
    val eventNumber: Long,
    val stampedSeq: Seq[Stamped[AnyKeyedEvent]],
    val isTransaction: Boolean,
    val since: Deadline,
    val isLastOfFlushedOrSynced: Boolean)
  extends Loggable

  private final class SubclassCache(superclassNames: Set[String]):
    private val cache = mutable.Map.empty[Class[?], Boolean]

    def contains(cls: Class[? <: Event]) =
      cache.getOrElseUpdate(cls,
        superclassesOf(cls)
          .iterator
          .map(_.simpleScalaName)
          .exists(superclassNames.contains))

    override def toString = s"SubclassCache($superclassNames)"

  private final case class PersistFrame(persist: Loggable,
    persistEventCount: Int, persistIndex: Int, persistCount: Int,
    committedAt: Deadline):
    val beforeLastEventNr = persist.eventNumber + persistEventCount - 2
    val duration = committedAt - persist.since
    var nr = persist.eventNumber
    var isFirstEvent = true
    var isLastEvent = true

    def persistMarker: Char =
      if persistCount == 1 then ' '
      else if persistIndex == 0 & isFirstEvent then '┌'
      else if persistIndex == persistCount - 1 & isLastEvent then '└'
      else '│'

    def transactionMarker(forTrace: Boolean): Char =
      if persistEventCount == 1 then ' '
      else if persist.isTransaction then
        if isFirstEvent then '⎛' // ⎧
        else if isLastEvent then '⎝' // ⎩
        else if forTrace && nr == beforeLastEventNr then '⎨'
        else '⎪'
      else if !forTrace then ' '
      else if isFirstEvent then '┌'
      else if isLastEvent then '└'
      else if nr == beforeLastEventNr then '┤'
      else '╵' // ┆╎

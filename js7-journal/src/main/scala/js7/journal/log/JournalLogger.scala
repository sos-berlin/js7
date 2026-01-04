package js7.journal.log

import com.typesafe.scalalogging.Logger
import java.util.Locale.ROOT
import js7.base.log.CorrelId
import js7.base.log.Logger.syntax.*
import js7.base.log.LoggingEscapeCodes.{isColorAllowed, resetColor}
import js7.base.time.ScalaTime.*
import js7.base.utils.Classes.superclassesOf
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventsObservedEvent, KeyedEvent, Stamped}
import js7.journal.configuration.JournalConf
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

  def logCommitted(
    stampedSeq: Seq[Stamped[AnyKeyedEvent]],
    eventNumber: Long,
    since: Deadline,
    clusterState: String,
    isTransaction: Boolean = false,
    isAcknowledged: Boolean = false)
  : Unit =
    logCommitted:
      Array:
        val persist = SimpleLoggablePersist(
          stampedSeq,
          eventNumber = eventNumber,
          since,
          isTransaction = isTransaction,
          clusterState = clusterState)
        persist.isAcknowledged = isAcknowledged
        persist
      .view

  def logCommitted(persists: IndexedSeqView[LoggablePersist]): Unit =
    if !suppressed && logger.isInfoEnabled then
      val committedAt = now
      val myPersists = dropEmptyPersists(persists)

      if logger.isDebugEnabled then
        logPersists(myPersists, committedAt):
          logCommittedEvents

      def isLoggable(stamped: Stamped[AnyKeyedEvent]) =
        val event = stamped.value.event
        infoLoggableEventClasses.contains(event.getClass) || !event.isSucceeded

      val loggablePersists = myPersists.filter(_.stampedSeq.exists(isLoggable))
      if loggablePersists.nonEmpty then
        logPersists(loggablePersists.toVector.view, committedAt, isLoggable):
          infoLogPersist

  def suppress(supressed: Boolean): Unit =
    this.suppressed = supressed

  private def dropEmptyPersists(persists: IndexedSeqView[LoggablePersist]): IndexedSeqView[LoggablePersist] =
    val dropLeft = persists.segmentLength(_.stampedSeq.isEmpty)
    val dropRight = persists.reverse.segmentLength(_.stampedSeq.isEmpty)
    persists.slice(dropLeft, persists.length - dropRight)

  private def logPersists(
    persists: IndexedSeqView[LoggablePersist],
    committedAt: Deadline,
    isLoggable: Stamped[AnyKeyedEvent] => Boolean = _ => true)
    (body: (PersistFrame, Stamped[AnyKeyedEvent]) => Unit)
  : Unit =
    CorrelId.isolate: logCorrelId =>
      for persist <- persists do
        //logCorrelId := persist.correlId
        val stampedSeq = persist.stampedSeq.filter(isLoggable)
        val frame = PersistFrame(persist, stampedSeq.length, committedAt)
        val stampedIterator = stampedSeq.iterator
        var hasNext = stampedIterator.hasNext
        while hasNext do
          val stamped = stampedIterator.next()
          hasNext = stampedIterator.hasNext
          frame.isLastEvent = !hasNext
          body(frame, stamped)
          frame.nr += 1
          frame.isFirstEvent = false

  private def logCommittedEvents(frame: PersistFrame, stamped: Stamped[AnyKeyedEvent]): Unit =
    import frame.*
    sb.clear()
    sb.append:
      if stamped.value.event.isInstanceOf[EventsObservedEvent] then
        '.'
      else
        ':'  // Allow simple filtering of log file: grep " - :" x.log
    //? sb.fillRight(5) { sb.append(nr) }
    sb.append(persistMarker)
    sb.fillRight(syncOrFlushWidth):
      import persist.{isAcknowledged, isFirstPersist, persistCount}
      if isLastEvent && persist.isLastOfFlushedOrSynced then
        sb.append(if isAcknowledged then ackSyncOrFlushString else syncOrFlushString)
      else if isFirstEvent && isFirstPersist && persistCount >= 2 then
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

    sb.append(transactionMarker(forEachEvent = true))
    sb.append(stamped.eventId)
    if stamped.value.key != NoKey then
      sb.append(' ')
      sb.append(stamped.value.key)
      sb.append(spaceArrow)

    locally:
      val event = stamped.value.event
      val eventString =
        if event.hasShortString then
          event.toShortString
        else
          event.toShortString.truncateWithEllipsis(200, firstLineOnly = true)
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

    logger.underlying.debug(sb.toString)

  private def infoLogPersist(frame: PersistFrame, stamped: Stamped[AnyKeyedEvent]): Unit =
    import frame.*
    import stamped.value.{event, key}
    sb.clear()
    sb.append:
      if frame.persist.isAcknowledged then
        "Event ✔"
      else
        frame.persist.clusterState match
          case "Empty" => "Event "
          case o => s"Event (⚠️ $o) "
    sb.append(transactionMarker(forEachEvent = false))
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

  def apply(conf: JournalConf): JournalLogger =
    new JournalLogger(
      syncOrFlushString =
        if !conf.syncOnCommit then
          "flush"
        else if conf.simulateSync.isDefined then
          "~sync"
        else
          "sync ",
      infoLogEvents = conf.infoLogEvents)


  private[journal] trait LoggablePersist:
    //def correlId: CorrelId
    def eventNumber: Long
    def stampedSeq: Seq[Stamped[AnyKeyedEvent]]
    def isTransaction: Boolean
    def since: Deadline
    def clusterState: String

    private[JournalLogger] final var persistIndex = 0
    private[JournalLogger] final var persistCount = 0
    private[JournalLogger] final var isFirstPersist = false
    private[JournalLogger] final var isLastPersist = false
    private[JournalLogger] final var isLastOfFlushedOrSynced = false
    //private[JournalLogger] final var isBeforeLast = false
    final var isAcknowledged: Boolean = false


  // Not copyable due to vars in LoggablePersist
  private final class SimpleLoggablePersist(
    //val correlId: CorrelId,
    val stampedSeq: Seq[Stamped[AnyKeyedEvent]],
    val eventNumber: Long,
    val since: Deadline,
    val isTransaction: Boolean,
    val clusterState: String)
  extends LoggablePersist:
    persistIndex = 0
    persistCount = 1
    isFirstPersist = true
    isLastPersist = true
    isLastOfFlushedOrSynced = true
    isLastOfFlushedOrSynced = true
    isAcknowledged = false

  private final class SubclassCache(superclassNames: Set[String]):
    private val cache = mutable.Map.empty[Class[?], Boolean]

    def contains(cls: Class[? <: Event]) =
      cache.getOrElseUpdate(cls,
        superclassesOf(cls)
          .iterator
          .map(_.simpleScalaName)
          .exists(superclassNames.contains))

    override def toString = s"SubclassCache($superclassNames)"


  private final case class PersistFrame(
    persist: LoggablePersist,
    persistEventCount: Int,
    committedAt: Deadline):

    val beforeLastEventNr = persist.eventNumber + persistEventCount - 2
    val duration = committedAt - persist.since
    var nr = persist.eventNumber
    var isFirstEvent = true
    var isLastEvent = false

    import persist.{isFirstPersist, isLastPersist, isTransaction}

    /** Mark a Chunk and LoggablePersists (collapsed persist operations) and
      * each end of LoggablePersist. */
    def persistMarker: Char =
      if isFirstPersist && isFirstEvent then // First line of chunk
        if isLastEvent then
          if isLastPersist then ' ' // Single event chunk
          else '╒' // Start of the Chunk and end of the first (single event) LoggablePersist
        else
          '┌' // Start of the Chunk
      else if isLastEvent then
        if isLastPersist then '└' // chunk ends
        else '├' // End of LoggablePersist
      else '│'

    def transactionMarker(forEachEvent: Boolean): Char =
      if isFirstEvent && isLastEvent then ' '
      else if isTransaction then
        if isFirstEvent then '⎛' // ⎧
        else if isLastEvent then '⎝' // ⎩
        else if forEachEvent && nr == beforeLastEventNr then '⎨'
        else '⎪'
      else if !forEachEvent then ' '
      else if isFirstEvent then '┌'
      else if isLastEvent then '└'
      else if isLastPersist && nr == beforeLastEventNr then '┤'
      else '┆' // ╵╷┊┆╎
  end PersistFrame


  def markChunkForLogging(chunk: fs2.Chunk[LoggablePersist]): Unit =
    val n = chunk.size
    if n > 0 then
      chunk(0).isFirstPersist = true
      for i <- 0 until n do
        val persist = chunk(i)
        persist.persistIndex = i
        persist.persistCount = n
      //if n >= 2 then chunk(n - 2).isBeforeLast = true

      var i = n
      val it = chunk.reverseIterator
      if it.exists: written =>
        i -= 1
        written.stampedSeq.nonEmpty // An existing event (not deleted due to commitLater)
      then
        chunk(i).isLastPersist = true
        chunk(i).isLastOfFlushedOrSynced = true

    //if it.exists: written =>
    //  i -= 1
    //  written.stampedSeq.nonEmpty // An existing event (not deleted due to commitLater)
    //then
    //  chunk(i).isBeforeLast = true

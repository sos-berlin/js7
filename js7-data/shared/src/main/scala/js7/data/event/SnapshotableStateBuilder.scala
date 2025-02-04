package js7.data.event

import cats.effect.IO
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.Problem
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{itemsPerSecondString, perSecondStringOnly}
import js7.base.time.Timestamp
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SetOnce
import js7.base.utils.StackTraces.*
import js7.data.cluster.ClusterState
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.SnapshotableStateBuilder.*
import scala.concurrent.duration.Deadline.now
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

trait SnapshotableStateBuilder[S <: SnapshotableState[S]]:
  protected val S: SnapshotableState.Companion[S]

  private val since = now
  private var recordCount = 1L
  private var _snapshotCount = 0L
  private var _firstEventId = EventId.BeforeFirst
  private var _eventId = EventId.BeforeFirst
  private var _eventCount = 0L
  private val _journalHeader = SetOnce[JournalHeader]
  private val getStatePromise = Promise[IO[S]]()

  def initializeState(
    journalHeader: Option[JournalHeader],
    eventId: EventId,
    totalEventCount: Long,
    state: S)
  : Unit =
    journalHeader foreach { _journalHeader := _ }
    _eventId = eventId
    _eventCount = totalEventCount - journalHeader.fold(0L)(_.totalEventCount)
    onInitializeState(state)
    onStateIsAvailable()

  protected def onInitializeState(state: S): Unit

  protected def onAddSnapshotObject: PartialFunction[Any, Unit]

  protected def onOnAllSnapshotsObjectsAdded(): Unit = {}

  protected def onAddEvent: PartialFunction[Stamped[KeyedEvent[Event]], Unit]

  def journalState: JournalState

  def clusterState: ClusterState

  def result(): S

  def addSnapshotObject(obj: Any): Unit =
    recordCount += 1
    _snapshotCount += 1
    obj match
      case journalHeader: JournalHeader =>
        try
          require(_firstEventId == EventId.BeforeFirst && _eventId == EventId.BeforeFirst,
            "EventId mismatch in snapshot")
          _journalHeader := journalHeader
          _firstEventId = journalHeader.eventId
          _eventId = journalHeader.eventId
        catch case NonFatal(t) =>
          throw new RuntimeException(
            s"Application of JournalHeader failed in record #$recordCount for $S", t)

      case SnapshotEventId(eventId) =>
        require(eventId == _firstEventId && eventId == _eventId ||
                _firstEventId == EventId.BeforeFirst && _eventId == EventId.BeforeFirst,
          "EventId mismatch in snapshot")
        _firstEventId = eventId
        _eventId = eventId

      case _ =>
        try onAddSnapshotObject.applyOrElse(obj, onSnapshotObjectNotApplicable)
        catch case NonFatal(t) =>
          throw new RuntimeException(
            s"Application of snapshot object '${obj.getClass.shortClassName}' failed " +
              s"in record #$recordCount for $S", t)

  protected def onSnapshotObjectNotApplicable(obj: Any): Unit =
    throw SnapshotObjectNotApplicableProblem(obj).throwable.appendCurrentStackTrace

  def onAllSnapshotObjectsAdded(): Unit =
    onOnAllSnapshotsObjectsAdded()
    onStateIsAvailable()

  private def onStateIsAvailable(): Unit =
    getStatePromise.success(IO:
      synchronized:
        result())

  final def addStampedEvent(stamped: Stamped[KeyedEvent[Event]]): Unit =
    synchronized:  // synchronize with asynchronous execution of synchronizedStateFuture
      try
        recordCount += 1
        if stamped.eventId <= _eventId then
          throw new IllegalArgumentException(s"EventId is not ascending: ${EventId.toString(_eventId)} >= ${stamped.toString.truncateWithEllipsis(100)}")
        try onAddEvent(stamped)
        catch case NonFatal(t) =>
          throw new RuntimeException(s"Event failed: $stamped", t)
        _eventCount += 1
        if _firstEventId == EventId.BeforeFirst then
          _firstEventId = stamped.eventId
        _eventId = stamped.eventId
      catch case NonFatal(t) =>
        throw new RuntimeException(
          s"${stamped.value.event.getClass.simpleScalaName} event failed in record #$recordCount for $S",
          t)

  def logStatistics(byteCount: Option[Long]): Unit =
    val elapsed = since.elapsed
    if elapsed >= 1.s then
      logger.debug:
        itemsPerSecondString(elapsed, _snapshotCount + eventCount, "snapshots+events") +
        byteCount.fold("")(byteCount =>
          ", " + perSecondStringOnly(elapsed, byteCount / 1_000_000, "MB", gap = false) +
          " " + toKBGB(byteCount)
        ) + " read"
    if snapshotCount + eventCount > 0 then
      val age = (Timestamp.now - EventId.toTimestamp(eventId)).withMillis(0).pretty
      logger.info(s"Recovered last EventId is ${EventId.toString(eventId)}, emitted $age ago " +
        s"($snapshotCount snapshot objects and $eventCount events" +
        (byteCount.fold("")(o => ", " + toKBGB(o))) +
        " read" +
        ((elapsed >= 10.s) ?? s" in ${elapsed.pretty}") +
        ")")

  def synchronizedStateFuture: Future[IO[S]] =
    getStatePromise.future

  /** Journal file's JournalHeader. */
  final def fileJournalHeader: Option[JournalHeader] =
    _journalHeader.toOption

  /** Calculated next JournalHeader. */
  final def nextJournalHeader: Option[JournalHeader] =
    _journalHeader.toOption.map(_.copy(
      eventId = eventId,
      totalEventCount = totalEventCount,
      totalRunningTime = _journalHeader.toOption.fold(ZeroDuration) { header =>
        val lastJournalDuration = lastEventIdTimestamp - header.timestamp
        (header.totalRunningTime + lastJournalDuration).roundUpToNext(1.ms)
      },
      timestamp = lastEventIdTimestamp))

  final def eventId: EventId = _eventId

  final def snapshotCount: Long =
    _snapshotCount

  final def eventCount: Long =
    _eventCount

  final def totalEventCount: Long =
    _journalHeader.toOption.fold(0L)(_.totalEventCount) + _eventCount

  private def lastEventIdTimestamp: Timestamp =
    if eventId == EventId.BeforeFirst then Timestamp.now
    else EventId.toTimestamp(eventId)


object SnapshotableStateBuilder:
  private val logger = Logger[this.type]

  abstract class Simple[S <: SnapshotableState[S]](protected val S: SnapshotableState.Companion[S])
  extends SnapshotableStateBuilder[S], StandardsBuilder:
    private var _state = S.empty

    protected def onInitializeState(state: S): Unit =
      _state = state

    override def addSnapshotObject(obj: Any): Unit = obj match
      case o: JournalState =>
        _state = _state.withStandards(_state.standards.copy(
          journalState = o))

      case o: ClusterState =>
        _state = _state.withStandards(_state.standards.copy(
          clusterState = o))

      case o => super.addSnapshotObject(o)

    protected def onAddEvent =
      case stamped =>
        _state = _state.applyKeyedEvent(stamped.value).orThrow

    def result(): S =
      _state.withEventId(eventId)

    protected final def state =
      _state

    protected final def updateState(state: S): Unit =
      _state = state

  private case class SnapshotObjectNotApplicableProblem(obj: Any) extends Problem.Coded:
    def arguments = Map(
      "object" -> obj.getClass.scalaName)

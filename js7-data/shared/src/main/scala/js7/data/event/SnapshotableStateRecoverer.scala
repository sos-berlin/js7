package js7.data.event

import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SetOnce
import js7.base.utils.StackTraces.*
import js7.data.cluster.ClusterState
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.SnapshotableStateRecoverer.*
import scala.util.control.NonFatal

trait SnapshotableStateRecoverer[S <: SnapshotableState[S]]:
  protected val S: SnapshotableState.Companion[S]

  private var recordCount = 1L
  private var _firstEventId = EventId.BeforeFirst
  private var _eventId = EventId.BeforeFirst
  private val _journalHeader = SetOnce[JournalHeader]

  protected def onAddSnapshotObject: PartialFunction[Any, Unit]

  def journalState: JournalState

  def clusterState: ClusterState

  def result(): S

  def addSnapshotObject(obj: Any): Unit =
    recordCount += 1
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

  /** Journal file's JournalHeader. */
  final def fileJournalHeader: Option[JournalHeader] =
    _journalHeader.toOption

  final def eventId: EventId =
    _eventId


object SnapshotableStateRecoverer:

  abstract class Simple[S <: SnapshotableState[S]](protected val S: SnapshotableState.Companion[S])
  extends SnapshotableStateRecoverer[S], StandardsRecoverer:
    private var _state = S.empty

    override def addSnapshotObject(obj: Any): Unit = obj match
      case o: JournalState =>
        _state = _state.withStandards(_state.standards.copy(
          journalState = o))

      case o: ClusterState =>
        _state = _state.withStandards(_state.standards.copy(
          clusterState = o))

      case o => super.addSnapshotObject(o)

    def result(): S =
      _state.withEventId(eventId)

    protected final def state =
      _state

    protected final def updateState(state: S): Unit =
      _state = state

  private case class SnapshotObjectNotApplicableProblem(obj: Any) extends Problem.Coded:
    def arguments = Map(
      "object" -> obj.getClass.scalaName)

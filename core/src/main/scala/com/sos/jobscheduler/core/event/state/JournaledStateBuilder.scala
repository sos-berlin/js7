package com.sos.jobscheduler.core.event.state

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.SetOnce
import com.sos.jobscheduler.base.utils.Strings._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.core.event.journal.data.JournalHeader
import com.sos.jobscheduler.core.event.state.JournaledStateBuilder._
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.{Event, EventId, JournalState, JournaledState, KeyedEvent, Stamped}
import monix.eval.Task
import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

trait JournaledStateBuilder[S <: JournaledState[S]]
{
  private val stopwatch = new Stopwatch
  private var _snapshotCount = 0L
  private var _firstEventId = EventId.BeforeFirst
  private var _eventId = EventId.BeforeFirst
  private var _eventCount = 0L
  private val _journalHeader = SetOnce[JournalHeader]
  private val getStatePromise = Promise[Task[S]]()

  def initializeState(journalHeader: Option[JournalHeader], eventId: EventId, totalEventCount: Long, state: S): Unit = {
    journalHeader foreach { _journalHeader := _ }
    _eventId = eventId
    _eventCount = totalEventCount - journalHeader.fold(0L)(_.totalEventCount)
    onInitializeState(state)
    onStateIsAvailable()
  }

  protected def onInitializeState(state: S): Unit

  protected def onAddSnapshot: PartialFunction[Any, Unit]

  protected def onOnAllSnapshotsAdded(): Unit

  protected def onAddEvent: PartialFunction[Stamped[KeyedEvent[Event]], Unit]

  def state: S

  def journalState: JournalState

  def clusterState: ClusterState

  def addSnapshot(snapshot: Any): Unit =
    snapshot match {
      case journalHeader: JournalHeader =>
        this._journalHeader := journalHeader
        require(_firstEventId == EventId.BeforeFirst && _eventId == EventId.BeforeFirst)
        _firstEventId = journalHeader.eventId
        _eventId = journalHeader.eventId

      case _ =>
        _snapshotCount += 1
        onAddSnapshot(snapshot)
    }

  def onAllSnapshotsAdded(): Unit = {
    onOnAllSnapshotsAdded()
    onStateIsAvailable()
  }

  private def onStateIsAvailable(): Unit =
    getStatePromise.success(Task {
      synchronized {
        state
      }
    })

  final def addEvent(stamped: Stamped[KeyedEvent[Event]]) =
    synchronized {  // synchronize with asynchronous execution of synchronizedStateFuture
      if (stamped.eventId <= _eventId) {
        throw new IllegalArgumentException(s"EventId out of order: ${EventId.toString(_eventId)} ≥ ${stamped.toString.truncateWithEllipsis(100)}")
      }
      try onAddEvent(stamped)
      catch { case NonFatal(t) =>
        throw new RuntimeException(s"Event failed: $stamped", t)
      }
      _eventCount += 1
      if (_firstEventId == EventId.BeforeFirst) {
        _firstEventId = stamped.eventId
      }
      _eventId = stamped.eventId
    }

  def logStatistics(): Unit = {
    if (stopwatch.duration >= 1.s) {
      logger.debug(stopwatch.itemsPerSecondString(_snapshotCount + eventCount, "snapshots+events") + " read")
    }
    if (eventCount > 0) {
      val age = (Timestamp.now - EventId.toTimestamp(eventId)).withMillis(0).pretty
      val t = (stopwatch.duration >= 10.s) ?: s" in ${stopwatch.duration.pretty}"
      logger.info(s"Recovered last EventId is ${EventId.toString(eventId)}, emitted $age ago " +
        s"($snapshotCount snapshot elements and $eventCount events read$t)")
    }
  }

  def synchronizedStateFuture: Future[Task[S]] =
    getStatePromise.future

  /** Journal file's JournalHeader. */
  final def fileJournalHeader = _journalHeader.toOption

  /** Calculated next JournalHeader. */
  final def recoveredJournalHeader: Option[JournalHeader] =
    _journalHeader.map(_.copy(
      eventId = eventId,
      totalEventCount = totalEventCount,
      totalRunningTime = _journalHeader.fold(Duration.Zero) { header =>
        val lastJournalDuration = EventId.toTimestamp(_eventId) - EventId.toTimestamp(_firstEventId)
        header.totalRunningTime + lastJournalDuration roundUpToNext 1.ms
      },
      timestamp = lastEventIdTimestamp))

  final def eventId = _eventId

  final def snapshotCount = _snapshotCount

  final def eventCount = _eventCount

  final def totalEventCount = _journalHeader.fold(0L)(_.totalEventCount) + _eventCount

  private def lastEventIdTimestamp: Timestamp =
    if (eventId == EventId.BeforeFirst) Timestamp.now
    else EventId.toTimestamp(eventId)
}

object JournaledStateBuilder
{
  private val logger = Logger(getClass)
}

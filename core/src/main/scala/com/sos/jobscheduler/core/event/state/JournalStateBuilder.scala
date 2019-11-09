package com.sos.jobscheduler.core.event.state

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Strings._
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.core.event.journal.data.JournalHeader
import com.sos.jobscheduler.core.event.state.JournalStateBuilder._
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.{Event, EventId, JournaledState, KeyedEvent, Stamped}
import scala.concurrent.duration.{Duration, FiniteDuration}

trait JournalStateBuilder[S <: JournaledState[S, E], E <: Event]
{
  private val stopwatch = new Stopwatch
  private var _snapshotCount = 0L
  private var _eventId = EventId.BeforeFirst
  private var _eventCount = 0L
  private val _journalHeader = SetOnce[JournalHeader]

  def initializeState(journalHeader: Option[JournalHeader], state: S): Unit = {
    journalHeader foreach { _journalHeader := _ }
    onInitializeState(state)
  }

  protected def onInitializeState(state: S): Unit

  protected def onAddSnapshot: PartialFunction[Any, Unit]

  def onAllSnapshotsAdded(): Unit

  protected def onAddEvent: PartialFunction[Stamped[KeyedEvent[Event]], Unit]

  def state: S

  def clusterState: ClusterState

  def addSnapshot(snapshot: Any): Unit =
    snapshot match {
      case journalHeader: JournalHeader =>
        this._journalHeader := journalHeader
        require(_eventId == EventId.BeforeFirst)
        _eventId = journalHeader.eventId

      case _ =>
        _snapshotCount += 1
        onAddSnapshot(snapshot)
    }

  final def addEvent(stamped: Stamped[KeyedEvent[Event]]) = {
    if (stamped.eventId <= _eventId) {
      throw new IllegalArgumentException(s"EventId out of order: ${EventId.toString(_eventId)} ≥ ${stamped.toString.truncateWithEllipsis(100)}")
    }
    onAddEvent(stamped)
    _eventCount += 1
    _eventId = stamped.eventId
  }

  def logStatistics(): Unit = {
    if (stopwatch.duration >= 1.s) {
      logger.debug(stopwatch.itemsPerSecondString(_snapshotCount + eventCount, "snapshots+events") + " read")
    }
    if (eventCount > 0) {
      val age = (Timestamp.now - EventId.toTimestamp(eventId)).withMillis(0).pretty
      logger.info(s"Recovered last EventId is ${EventId.toString(eventId)}, issued $age ago " +
        s"($snapshotCount snapshot elements and $eventCount events read in ${stopwatch.duration.pretty})")
    }
  }

  /** Journal file's JournalHeader. */
  final def fileJournalHeader = _journalHeader.toOption

  /** Calculated next JournalHeader. */
  final def recoveredJournalHeader: Option[JournalHeader] =
    _journalHeader.map(o => o.copy(
      eventId = eventId,
      totalEventCount = totalEventCount,
      timestamp = lastEventIdTimestamp,
      totalRunningTime = totalRunningTime))

  final def eventId = _eventId

  final def snapshotCount = _snapshotCount

  final def eventCount = _eventCount

  final def totalEventCount = _journalHeader.fold(0L)(_.totalEventCount) + _eventCount

  /** With recovery time added. */
  final def totalRunningTime: FiniteDuration =
    if (eventId == EventId.BeforeFirst)
      Duration.Zero
    else
      _journalHeader.fold(Duration.Zero)(o => o.totalRunningTime + (lastEventIdTimestamp - o.timestamp))

  private def lastEventIdTimestamp: Timestamp =
    if (eventId == EventId.BeforeFirst) Timestamp.now
    else EventId.toTimestamp(eventId)
}

object JournalStateBuilder
{
  private val logger = Logger(getClass)
}

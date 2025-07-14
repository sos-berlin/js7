package js7.journal

import cats.effect.{IO, Resource, ResourceIO}
import js7.base.log.Logger
import js7.base.metering.CallMeter
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.JournalEvent.SnapshotTaken
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalEvent, KeyedEvent, SnapshotableState, Stamped}
import js7.journal.FileJournal.*
import js7.journal.Snapshotter.*
import js7.journal.files.JournalFiles.extensions.*
import js7.journal.write.{EventJournalWriter, SnapshotJournalWriter}
import scala.concurrent.duration.Deadline
import scala.language.unsafeNulls

transparent trait Snapshotter[S <: SnapshotableState[S]]:
  this: FileJournal[S] =>

  private var _lastSnapshotTakenEventId = EventId.BeforeFirst

  protected final def lastSnapshotTakenEventId = _lastSnapshotTakenEventId

  protected final def eventWriterResource(isStarting: Boolean): ResourceIO[EventJournalWriter] =
    Resource:
      state.updateWithResult: state =>
        startNewJournalFile(state, isStarting = isStarting)
          .map: (aggregate, allocatedEventWriter) =>
            S.updateStaticReference(aggregate)
            val state_ = state.copy(
              uncommitted = aggregate,
              committed = aggregate,
              totalEventCount = state.totalEventCount + 1 /*SnapshotTaken*/)
            state_ -> allocatedEventWriter

  private def startNewJournalFile(state: State[S], isStarting: Boolean)
  : IO[(S, (EventJournalWriter, IO[Unit]))] =
    IO.defer:
      val since = Deadline.now
      assertNothingIsUncommitted(state)
      for
        (fileEventId, fileLengthBeforeEvents, snapshotTaken, aggregate) <- writeSnapshot(state)
        (eventWriter, release) <-
          eventWriterResource(fileEventId = fileEventId, snapshotTaken).allocated
        _ <- IO:
          eventWriter.onJournalingStarted(fileLengthBeforeEvents = fileLengthBeforeEvents)
        _ <- onSnapshotTaken(eventWriter, snapshotTaken, eventNumber = state.totalEventCount, since,
          isStarting = isStarting)
        _ <- IO:
          val how = if conf.syncOnCommit then "(with sync)" else "(without sync)"
          logger.debug(s"Snapshot written $how to journal file ${eventWriter.file.getFileName}")
      yield
        (aggregate, eventWriter -> release)

  private def assertNothingIsUncommitted(state: State[S]): Unit =
    if state.uncommitted ne state.committed then
      if state.uncommitted == state.committed then
        logger.error("💥 state.uncommitted ne state.committed DESPITE state.uncommitted == state.committed")
      else
        logger.error("💥 state.uncommitted != state.committed")
      logger.info(s"state.uncommitted=⏎")
      state.uncommitted.emitLineStream(logger.info(_))
      logger.info(s"state.committed=⏎")
      state.committed.emitLineStream(logger.info(_))
      throw new AssertionError("Snapshotter: state.uncommitted != state.committed")

  private def eventWriterResource(
    fileEventId: EventId,
    snapshotTaken: Stamped[KeyedEvent[SnapshotTaken]])
  : ResourceIO[EventJournalWriter] =
    Resource.make(
      acquire =
        IO.blocking:
          val eventWriter = new EventJournalWriter(
            journalLocation,
            fileEventId = fileEventId,
            after = snapshotTaken.eventId,
            journalId, journalingObserver, bean,
            simulateSync = conf.simulateSync,
            initialEventCount = 1 /*SnapshotTaken has been written*/)
          journalLocation.updateSymbolicLink(eventWriter.file)
          eventWriter)(
      release = eventWriter =>
        IO.blocking:
          if isSwitchedOver || _suppressSnapshotWhenStopping then
            eventWriter.flush(sync = false)
            eventWriter.close()
          else
            eventWriter.closeProperly(sync = conf.syncOnCommit))

  protected final def writeSnapshot(state: State[S])
  : IO[(EventId, Long, Stamped[KeyedEvent[SnapshotTaken]], S)] =
    IO.defer:
      assertThat(state.uncommitted eq state.committed)
      val snapshotTaken = eventIdGenerator.stamp:
        NoKey <-: JournalEvent.SnapshotTaken
      val aggregate = state.committed.applyStampedEvents(snapshotTaken :: Nil).orThrow

      val file = journalLocation.file(after = state.committed.eventId)
      val journalHeader = lastJournalHeader.nextGeneration[S](
        eventId = state.committed.eventId,
        totalEventCount = state.totalEventCount,
        totalRunningTime = totalRunningTime.roundUpToNext(1.ms),
        timestamp = clock.now())
      logger.info(s"Starting new journal file #${journalHeader.generation} ${file.getFileName
        } with a snapshot ${if conf.syncOnCommit then "(using sync)" else "(no sync)"}")
      logger.debug(journalHeader.toString)

      val checkingRecoverer = conf.slowCheckState ? S.newRecoverer()
      for
        fileLengthBeforeEventsAndEventId <-
          SnapshotJournalWriter.writeSnapshotStream(
            S, file, journalHeader,
            state.committed.toSnapshotStream.map: o =>
              checkingRecoverer.foreach(_.addSnapshotObject(o))
              o,
            snapshotTaken,
            bean,
            syncOnCommit = conf.syncOnCommit,
            simulateSync = conf.simulateSync)
        _ <- IO:
          checkingRecoverer.foreach: recoverer => // Simulate recovery
            val recoveredAggregate = recoverer.result().withEventId(state.committed.eventId)
            assertEqualSnapshotState("Written snapshot", state.committed, recoveredAggregate)
      yield
        bean.fileSize = fileLengthBeforeEventsAndEventId.position // Not accurate, without event
        lastJournalHeader = journalHeader
        _lastSnapshotTakenEventId = snapshotTaken.eventId
        (fileLengthBeforeEventsAndEventId.value,
          fileLengthBeforeEventsAndEventId.position,
          snapshotTaken,
          aggregate)


object Snapshotter:
  private val logger = Logger[this.type]
  private val meteter = CallMeter("journal.snapshot.duration.total")

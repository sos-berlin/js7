package js7.journal

import cats.effect.IO
import js7.base.catsutils.CatsEffectExtensions.True
import js7.base.catsutils.CatsExtensions.ifTrue
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AsyncLock
import js7.base.utils.CatsUtils.syntax.logWhenMethodTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.JournalEvent.SnapshotTaken
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalEvent, KeyedEvent, SnapshotableState, Stamped}
import js7.journal.FileJournal.*
import js7.journal.Snapshotter.*
import js7.journal.files.JournalFiles.extensions.*
import js7.journal.write.SnapshotJournalWriter
import scala.language.unsafeNulls

transparent trait Snapshotter[S <: SnapshotableState[S]]:
  this: FileJournal[S] =>

  private val snapshotLock = AsyncLock()
  private var _lastSnapshotTakenEventId = EventId.BeforeFirst

  private final def lastSnapshotTakenEventId = _lastSnapshotTakenEventId

  protected final def snapshotPeriodically: IO[Unit] =
    logger.traceIO:
      restartSnapshotTimerSignal.discrete.as(false)
        .keepAlive(conf.snapshotPeriod, IO.True)
        .filter(identity) // let through keep-alives
        .interruptWhenF(untilStopRequested)
        .evalMap: _ =>
          IO.defer:
            logger.debug:
              s"takeSnapshot because period of ${conf.snapshotPeriod.pretty} has elapsed"
            takeSnapshot(dontSignal = ())
        .compile.drain

  final def takeSnapshot: IO[Unit] =
    takeSnapshot(dontSignal = ()) *>
      restartSnapshotTimerSignal.set(())

  /** @param dontSignal To make the difference to argumentless takeSignal clear. */
  protected final def takeSnapshot(ignoreIsStopping: Boolean = false, dontSignal: Unit)
  : IO[Unit] =
    // A snapshot is taken through stopping and starting the Committer.
    logger.debugIO:
      snapshotLock.lock:
        // The new snapshot's EventId must differ from the last snapshot's EventId,
        // otherwise no snapshot is taken, and the committer continues.
        // A SnapshotTaken event at the beginning of a journal file increments the EventId.
        IO:
          (!isStopping || ignoreIsStopping) &&
            state.get.committed.eventId > lastSnapshotTakenEventId
        .ifTrue:
          IO.uncancelable: _ => // Uncancelable !!!
            stopCommitter >>
              IO.whenA(!isStopping || ignoreIsStopping):
                startCommitter(isStarting = false)
        .logWhenMethodTakesLonger

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
        (fileSize, fileLengthBeforeEventsAndEventId) <-
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
        bean.fileSize = fileSize
        lastJournalHeader = journalHeader
        _lastSnapshotTakenEventId = snapshotTaken.eventId
        (fileLengthBeforeEventsAndEventId.value,
          fileLengthBeforeEventsAndEventId.position,
          snapshotTaken,
          aggregate)


object Snapshotter:
  private val logger = Logger[this.type]

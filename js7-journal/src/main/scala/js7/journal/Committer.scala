package js7.journal

import cats.effect
import cats.effect.kernel.{DeferredSink, Outcome}
import cats.effect.{Deferred, IO, Resource, ResourceIO}
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.Chunk
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.{False, addElapsedToAtomicNanos, startAndForget}
import js7.base.catsutils.CatsEffectUtils.{outcomeToEither, whenDeferred}
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.metering.CallMeter
import js7.base.monixutils.AsyncVariable
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatalFlatten
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.CatsUtils.syntax.{RichResource, logWhenMethodTakesLonger, whenItTakesLonger}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, Atomic}
import js7.common.jsonseq.PositionAnd
import js7.data.Problems.ClusterNodeHasBeenSwitchedOverProblem
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost, ClusterResetStarted, ClusterSwitchedOver}
import js7.data.cluster.ClusterState
import js7.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import js7.data.event.TimestampedKeyedEvent.{keyedEvent, maybeMillisSinceEpoch}
import js7.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, SnapshotableState, Stamped, TimeCtx}
import js7.journal.Committer.*
import js7.journal.FileJournal.*
import js7.journal.files.JournalFiles.extensions.updateSymbolicLink
import js7.journal.log.JournalLogger
import js7.journal.log.JournalLogger.LoggablePersist
import js7.journal.problems.Problems.JournalKilledProblem
import js7.journal.write.EventJournalWriter
import scala.concurrent.duration.Deadline
import scala.language.unsafeNulls

/** Provides the nested class CommitterService. */
transparent trait Committer[S <: SnapshotableState[S]]:
  this: FileJournal[S] =>

  private val currentService = AsyncVariable(none[Allocated[IO, CommitterService]])
  private var _isSwitchedOver = false
  private val committerTerminatedUnexpectedly = Deferred.unsafe[IO, Either[Throwable, Unit]]
  private val journalLogger = JournalLogger(conf)
  private val statistics = new Statistics

  protected final def logStatistics(): Unit =
    statistics.logLine.foreach:
      logger.debug

  protected final def whenCommitterTerminatedUnexpectedly: IO[Either[Throwable, Unit]] =
    committerTerminatedUnexpectedly.get

  protected final def isSwitchedOver =
    _isSwitchedOver

  protected final def startCommitter(isStarting: Boolean): IO[Unit] =
    currentService.update:
      case Some(_) => IO.raiseError:
        new IllegalStateException("startCommitter but Committer is already running")
      case None =>
        CommitterService.service(isStarting = isStarting)
          .toAllocated
          .productL:
            releaseObsoleteEvents
          .map(Some(_))
    .void

  protected final def stopCommitter: IO[Unit] =
    currentService.update:
      case None => IO.none
      case Some(allocated) => allocated.release.as(None)
    .void
    .logWhenMethodTakesLonger

  /** Release a concurrent persist operation, which waits for the missing acknowledgement and
    * blocks the persist lock.
    *
    * THIS AVOIDS A DEADLOCK.
    *
    * MUST BE CALLED WHEN PASSIVE NODE IS LOST:
    * - before emitting ClusterPassiveLost
    * - on ClusterCommand.ClusterPassiveDown (when waiting for ack for ClusterActiceNodeShutDown)
    */
  final def onPassiveLost: IO[Unit] =
    noMoreAcks("onPassiveLost")


  // CommitterService //

  /** The CommitterService writes a single journal file, starting with a snapshot. */
  private final class CommitterService(eventWriter: EventJournalWriter)
  extends Service.StoppableByRequest:
    import CommitterService.waitForAck

    private val processingPassiveLost = Atomic(0)
    private val snapshotBecauseBig = Atomic(false)

    override def toString = "CommitterService"

    protected def start =
      startService:
        IO.uncancelable: _ =>
          (untilStopRequested *> journalQueue.offer(None))
            .background.surround:
              runCommitPipeline
            .guaranteeCase: outcome =>
              IO.unlessA(isStopping):
                whenBeingKilled.tryGet.map(_.isDefined).flatMap: isKilling =>
                  // Warning might be duplicate with Service warnings?
                  outcome match
                    case Outcome.Succeeded(_) =>
                      logger.warn:
                        if isKilling then "Committer has been killed"
                        else "Committer terminated unexpectedly"
                    case Outcome.Errored(t) =>
                      logger.warn(s"Committer terminated unexpectedly with ${t.toStringWithCauses}")
                    case Outcome.Canceled() =>
                      logger.warn("Committer has been cancelled")
                  IO.whenA(!isKilling):
                    committerTerminatedUnexpectedly.complete:
                      outcomeToEither(outcome).rightAs(())
                    .void

    private def runCommitPipeline: IO[Unit] =
      logger.traceIO:
        fs2.Stream.fromQueueNoneTerminated(journalQueue, conf.concurrentPersistLimit)
          .through(commitPipeline)
          .interruptWhenF(whenBeingKilled.get)
          .compile.drain

    // The pipeline //
    private def commitPipeline: fs2.Pipe[IO, QueueEntry[S], Nothing] = _
      .chunks
      .evalMap: chunk =>
        // Try to preserve the chunks for faster processing!
        chunk.flatTraverse: queueEntry =>
          applyQueueEntryEvents(queueEntry)
        .addElapsedToAtomicNanos(bean.eventCalcNanos)
      .unchunks
      .conflateChunks(conf.concurrentPersistLimit) // Process two chunks concurrently
      //.pipeIf(conf.delay.isPositive):
      //  _.groupWithin(Int.MaxValue, conf.delay).unchunks
      .evalMap: chunk =>
      // TODO use (commitOptions.delay max conf.delay) - options.alreadyDelayed
      //<editor-fold desc="// delay code ...">
      //  val delay =
      //    chunk.iterator.map: applied =>
      //      (applied.commitOptions.delay max conf.delay) - applied.commitOptions.alreadyDelayed
      //    .maxOption.getOrElse(ZeroDuration)
      //  IO.whenA(delay.isPositive):
      //    logger.trace(s"### delay ${delay.pretty}")
      //    IO.sleep(delay)
      //  .productR:
      //</editor-fold>
          writeToFile(chunk)
            .addElapsedToAtomicNanos(bean.jsonWriteNanos)
        // maybe optimize: flush/commit concurrently, but only whole lines must be flushed
      // OrderStdoutEvents are removed from Written
      .unchunks
      .prefetchN(conf.concurrentPersistLimit) // Process three chunks concurrently //
      .evalMap: written =>
        val clusterState = written.aggregate.clusterState
        IO.unlessA(clusterState.isInstanceOf[ClusterState.Coupled]):
          // Do not switch requireClusterAck on, because
          // TODO ClusterResetStarted doesn't leave the Coupled state
          noMoreAcks(clusterState.getClass.getSimpleName)
        .productR:
          waitForAck(eventId = written.eventId, lastStamped = written.lastStamped)
        .map: isAck =>
          written.isAcknowledged = isAck
          written
        .productL:
          IO.defer:
            S.updateStaticReference(written.aggregate)
            state.updateDirect:
              _.copy(committed = written.aggregate)
      .chunks
      .evalTap: chunk =>
        IO.defer:
          val writtenView = chunk.asSeq.view
          val eventCount = writtenView.map(_.eventCount).sum
          if chunk.nonEmpty then
            bean.addEventCount(eventCount)
            val persistCount = chunk.size
            val now = System.nanoTime
            bean.persistTotal += persistCount
            bean.persistNanos += writtenView.map(w => now - w.since.time.toNanos).sum
            bean.commitTotal += persistCount // Would differ if commitLater is implemented
            statistics.onPersisted(persistCount = persistCount, eventCount,
              since = chunk.iterator.map(_.since).min)
            journalLogger.logCommitted(writtenView)

            eventWriter.onCommitted(chunk.last.get.positionAndEventId, n = eventCount)
          end if
          chunk.traverse: written =>
            written.completePersistOperation
          .map(_.fold)
      .evalTap: chunk =>
        // flushed.stampedKeyedEvents have been dropped when commitOptions.commitLater
        chunk.flatMap(o => Chunk.from(o.stampedKeyedEvents)).traverse: stamped =>
          possiblySwitchAck(stamped)
      .evalTap: chunk =>
        IO.whenA(shouldReleaseObsoleteEvents(chunk)):
          releaseObsoleteEvents
      .evalMap: _ =>
        maybeDoASnasphot
      .drain

    private def applyQueueEntryEvents(queueEntry: QueueEntry[S]): IO[Chunk[Applied]] =
      state.updateCheckedWithResult: state =>
        IO:
          if _isSwitchedOver then Left(ClusterNodeHasBeenSwitchedOverProblem)
          else if isBeingKilled then
            Left(JournalKilledProblem)
          else
            applyEvents(state, queueEntry).map: (aggregate, applied) =>
              state.copy(
                uncommitted = aggregate,
                totalEventCount = state.totalEventCount + applied.stampedKeyedEvents.size
              ) -> applied
      .flatMap:
        case Left(problem) =>
          queueEntry.completePersistedWithProblem(problem).as(Chunk.empty)
        case Right(applied_) =>
          val applied = applied_ : Applied /*IntelliJ 2025.1*/
          if applied.stampedKeyedEvents.isEmpty then
            // When no events, exit early //
            (applied.completeApplied *> applied.completePersistOperation)
              .as(Chunk.empty)
          else
            applied.stampedKeyedEvents.lastOption.map(_.value.event).match
              case Some(_: ClusterSwitchedOver) =>
                IO:
                  logger.debug("ClusterSwitchedOver: no more events are accepted")
                  _isSwitchedOver = true
              case Some(_: ClusterPassiveLost) =>
                processingPassiveLost += 1
                // onPassiveLost should already have switched off acks
                noMoreAcks("ClusterPassiveLost")
              case _ => IO.unit
            .productR:
              applied.completeApplied
            .as:
              Chunk.singleton(applied)

    private def applyEvents(state: State[S], queueEntry: QueueEntry[S]): Checked[(S, Applied)] =
      catchNonFatalFlatten:
        queueEntry.eventCalc.calculate(state.uncommitted, TimeCtx(clock.now()))
      .map: coll =>
        if coll.hasEvents then
          assertIsRecoverable(state.uncommitted, coll.keyedEvents)
        val stamped = coll.timestampedKeyedEvents.map: o =>
          eventIdGenerator.stamp(o.keyedEvent, timestampMillis = o.maybeMillisSinceEpoch)
        val aggregate = stamped.lastOption.fold(coll.aggregate): last =>
          coll.aggregate.withEventId(last.eventId)
        val applied = Applied(
          coll.originalAggregate, stamped, aggregate, eventNumber = state.totalEventCount + 1,
          queueEntry.commitOptions, queueEntry.since, queueEntry.metering,
          queueEntry.whenApplied, queueEntry.whenPersisted)
        (aggregate, applied)

    private def writeToFile(chunk: Chunk[Applied]): IO[Chunk[Written]] =
      IO.blocking:
        // TODO parallelize JSON serialization properly!
        chunk.map: applied =>
          import applied.{commitOptions, stampedKeyedEvents}
          val positionAndEventId =
            meterJsonWrite:
              eventWriter.writeEvents(stampedKeyedEvents, transaction = commitOptions.transaction)
          Written.fromApplied(applied, positionAndEventId)
      .flatTap: chunk =>
        IO.blocking:
          val t = Deadline.now
          eventWriter.flush(sync = conf.syncOnCommit)
          bean.fileSize = eventWriter.bytesWritten
          JournalLogger.markChunkForLogging(chunk)

    private def possiblySwitchAck(stamped: Stamped[AnyKeyedEvent]): IO[Unit] =
      stamped.value.event match
        case _: ClusterCoupled =>
          IO.defer:
            if processingPassiveLost.get() > 0 then
              IO:
                logger.warn("ClusterCoupled but ClusterPassiveLost is enqueued")
            else
              logger.info:
                "Cluster is coupled: Start requiring acknowledgements from passive cluster node"
              requireClusterAck.set(true)
                .productR(IO:
                  releaseEventIdsAfterClusterCoupledAck = Some(stamped.eventId))

        case event: ClusterPassiveLost =>
          IO:
            processingPassiveLost -= 1
            // requireClusterAck should already be false

        case event: (/*ClusterFailedOver | ClusterSwitchedOver | ClusterActiveNodeShutDown
          | */ClusterResetStarted) =>
          // TODO ClusterResetStart should switch to something like ClusterState.Reset
          noMoreAcks(event.getClass.getSimpleName)

        case _ => IO.unit

    private def shouldReleaseObsoleteEvents(chunk: Chunk[Written]): Boolean =
      chunk.exists: written =>
        written.stampedKeyedEvents.exists: stamped =>
          stamped.value.event match
            // (But SnapshotTaken does not run through the committer)
            case SnapshotTaken | _: JournalEventsReleased => true
            case _ => false

    private def maybeDoASnasphot: IO[Unit] =
      IO.defer:
        IO.whenA(!isStopping && isJournalFileTooBig):
          IO.unlessA(snapshotBecauseBig.getAndSet(true)):
            logger.debug(s"takeSnapshot because written size ${toKBGB(eventWriter.bytesWritten)
              } is above the limit ${toKBGB(conf.fileSizeLimit)}")
            // takeSnapshot stops this Committer and starts new one. This is recursive !!!
            takeSnapshot
              .guarantee(IO:
                snapshotBecauseBig := false)
              // Must run concurrently to avoid a deadlock due to recursion (?)
              .startAndForget

    private def isJournalFileTooBig =
      eventWriter.bytesWritten >= conf.fileSizeLimit &&
        eventWriter.eventCount >= 2 * estimatedSnapshotSize()

    private val estimatedSnapshotSize = new (() => Long):
      private var lastEventCount = Long.MinValue
      private var cachedResult = 0L

      def apply(): Long =
        // Avoid call estimatedSnapshotSize too often
        if eventWriter.eventCount > lastEventCount + conf.snapshotSizeEstimateEventThreshold then
          lastEventCount = eventWriter.eventCount
          cachedResult =
            meterEstimatedSnapshotSize:
              state.get.committed.estimatedSnapshotSize
        cachedResult
  end CommitterService

  private object CommitterService:
    def service(isStarting: Boolean): ResourceIO[CommitterService] =
      for
        eventWriter <- journalFileWriterCompleteResource(isStarting = isStarting)
        committerService <- Service.resource(new CommitterService(eventWriter))
      yield
        committerService

    private final def journalFileWriterCompleteResource(isStarting: Boolean)
    : ResourceIO[EventJournalWriter] =
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
          (fileEventId, fileLengthBeforeEvents, snapshotTaken, aggregate) <-
            writeSnapshot(state)
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

    private final def onSnapshotTaken(
      eventWriter: EventJournalWriter,
      snapshotTaken: Stamped[KeyedEvent[SnapshotTaken]],
      eventNumber: Long,
      since: Deadline,
      isStarting: Boolean)
    : IO[Unit] =
      IO.defer:
        if isStarting then
          // This is because after starting a Coupled active node,
          // the passive node is still not ready to acknowledged,
          // instead we get a deadlock with endless ClusterRecouple retries
          // TODO Maybe issue an PassiveLost after first SnapshotToken after actice node start,
          //  to change to ClusterState.PassiveLost ?
          IO.False
        else
          waitForAck(snapshotTaken.eventId, Some(snapshotTaken))
      .flatMap: isAcknowledged =>
        IO:
          bean.addEventCount(1)
          statistics.onPersisted(persistCount = 1, eventCount = 1, since)
          journalLogger.logCommitted(snapshotTaken :: Nil,
            eventNumber = eventNumber,
            since,
            clusterState = unsafeAggregate().clusterState.getClass.simpleScalaName,
            isAcknowledged = isAcknowledged)
          eventWriter.onCommitted(eventWriter.fileLengthAndEventId, n = 1)

    /** Wait for acknowledgement of passive cluster node.
      *
      * On ClusterPassiveLost terminate with false. */
    private def waitForAck(eventId: EventId, lastStamped: Option[Stamped[AnyKeyedEvent]])
    : IO[Boolean] =
      requireClusterAck.get.flatMap: requireAck =>
        if !requireAck then
          IO.False
        else
          logWaitingForAck(eventId, lastStamped):
            meterAck:
              ackSignal.waitUntil(_ >= eventId).as(true)
                .addElapsedToAtomicNanos(bean.ackNanos)
          .race:
            // Cancel waiting on ClusterPassiveLost
            requireClusterAck.waitUntil(!_) *> IO:
              logger.debug("requireClusterAck has become false, now cancel waiting for acknowledgement")
              false
          .map(_.merge)

    private def logWaitingForAck[A](eventId: EventId, lastStamped: Option[Stamped[AnyKeyedEvent]])
      (body: IO[A])
    : IO[A] =
      IO.defer:
        val sym = new BlockingSymbol
        val since = Deadline.now
        body
          .whenItTakesLonger(conf.ackWarnDurations): elapsed =>
            //val lastStampedEvent = persistBuffer.view
            //  .collect { case o: StandardPersist => o }
            //  .takeWhile(_.since.elapsed >= ackWarnMinimumDuration)
            //  .flatMap(_.stampedSeq).lastOption.fold("(unknown)")(_
            //  .toString.truncateWithEllipsis(200))
            ackSignal.get.flatMap: lastAck =>
              IO:
                sym.onWarn()
                val event = lastStamped.fold(""): stamped =>
                  s" ${stamped.value.event.getClass.simpleScalaName}"
                logger.warn(s"$sym Waiting for ${elapsed.pretty
                } for acknowledgement from passive cluster node of events until $eventId${event
                }, last acknowledged: ${EventId.toString(lastAck)}")
          .productL:
            whenDeferred(sym.warnLogged):
              IO:
                logger.info(s"🟢 Events until $eventId have finally been acknowledged after ${
                  since.elapsed.pretty}")
          .productL:
            IO(sym.clear())
  end CommitterService // object


  private sealed trait AppliedOrFlushed:
    val originalAggregate: S
    val stampedKeyedEvents: Vector[Stamped[AnyKeyedEvent]]
    val aggregate: S
    val commitOptions: CommitOptions
    val metering: CallMeter.Metering
    protected val whenPersisted: DeferredSink[IO, Checked[Persisted[S, Event]]]

    def nonEmpty: Boolean

    final def completePersistOperation: IO[Unit] =
      IO.defer:
        FileJournal.persistMeter.stopMetering(metering)
        whenPersisted.complete(Right(persisted)).void

    protected final def persisted: Persisted[S, Event] =
      Persisted(originalAggregate, stampedKeyedEvents, aggregate)

    //def traceLog(): Unit =
    //  val prefix = if this.isInstanceOf[Written] then "✔" else "+"
    //  stampedKeyedEvents.foreachWithBracket(
    //    if commitOptions.transaction then Round else Square
    //  ): (o, br) =>
    //    logger.trace(s"$prefix$br$o")


  /** Events of an Persist have been applied to State#uncommitted. */
  private final case class Applied(
    originalAggregate: S,
    stampedKeyedEvents: Vector[Stamped[AnyKeyedEvent]],
    aggregate: S,
    eventNumber: Long,
    commitOptions: CommitOptions,
    since: Deadline,
    metering: CallMeter.Metering,
    whenApplied: DeferredSink[IO, Checked[Persisted[S, Event]]],
    whenPersisted: DeferredSink[IO, Checked[Persisted[S, Event]]])
  extends AppliedOrFlushed:
    def nonEmpty = stampedKeyedEvents.nonEmpty

    def completeApplied: IO[Unit] =
      whenApplied.complete(Right(persisted))
        .void


  /** Events of an Persist have been written and flushed to the journal file. */
  private final case class Written private/*not copyable due to vars in LoggablePersist*/(
    eventId: EventId,
    eventCount: Int,
    lastStamped: Option[Stamped[AnyKeyedEvent]],
    originalAggregate: S,
    stampedKeyedEvents: Vector[Stamped[AnyKeyedEvent]],
    aggregate: S,
    since: Deadline,
    eventNumber: Long,
    commitOptions: CommitOptions,
    metering: CallMeter.Metering,
    whenPersisted: DeferredSink[IO, Checked[Persisted[S, Event]]],
    positionAndEventId: PositionAnd[EventId])
  extends AppliedOrFlushed with LoggablePersist:
    def isTransaction = commitOptions.transaction
    def stampedSeq = stampedKeyedEvents
    def nonEmpty = eventCount > 0
    def clusterState: String = aggregate.clusterState.getClass.simpleScalaName

  private object Written:
    def fromApplied(
      applied: Applied,
      positionAndEventId: PositionAnd[EventId])
    : Written =
      val lastStamped = applied.stampedKeyedEvents.lastOption
      Written(
        eventId = lastStamped.fold(applied.aggregate.eventId)(_.eventId),
        eventCount = applied.stampedKeyedEvents.size,
        lastStamped,
        applied.originalAggregate,
        if applied.commitOptions.commitLater then
          // Reduce heap usage. The (OrderStdWritten) events will neither be logged nor returned.
          Vector.empty
        else
          applied.stampedKeyedEvents,
        applied.aggregate, applied.since, applied.eventNumber, applied.commitOptions,
        applied.metering,
        applied.whenPersisted,
        positionAndEventId)


private[journal] object Committer:
  private val logger = Logger[this.type]
  private val meterEstimatedSnapshotSize = CallMeter("FileJournal.estimatedSnapshotSize")
  private val meterJsonWrite = CallMeter("FileJournal.jsonWrite")
  private val meterAck = CallMeter("FileJournal.ack")

package js7.journal

import cats.effect.kernel.{DeferredSink, DeferredSource}
import cats.effect.std.Queue
import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, FiberIO, IO, Resource, ResourceIO}
import cats.syntax.flatMap.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.Chunk
import fs2.concurrent.SignallingRef
import izumi.reflect.Tag
import java.nio.file.Files.{delete, exists}
import java.nio.file.{Files, Path, Paths}
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.catsutils.Environment
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.metering.CallMeter
import js7.base.monixutils.AsyncVariable
import js7.base.problem.Checked.{catchNonFatalFlatten, *}
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.time.WallClock
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.whenItTakesLonger
import js7.base.utils.MultipleLinesBracket.{Round, Square, zipWithBracket}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.base.utils.{Assertions, AsyncLock, Atomic, MultipleLinesBracket, Tests}
import js7.common.jsonseq.PositionAnd
import js7.data.Problems.ClusterNodeHasBeenSwitchedOverProblem
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeShutDown, ClusterCoupled, ClusterFailedOver, ClusterPassiveLost, ClusterResetStarted, ClusterSwitchedOver}
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.TimestampedKeyedEvent.{keyedEvent, maybeMillisSinceEpoch}
import js7.data.event.{AnyKeyedEvent, Event, EventCalc, EventDrivenState, EventId, JournalEvent, JournalHeader, JournalId, KeyedEvent, SnapshotableState, Stamped, TimeCtx}
import js7.journal.Journaler.*
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.extensions.*
import js7.journal.log.JournalLogger.Loggable
import js7.journal.recover.Recovered
import js7.journal.watch.JournalingObserver
import js7.journal.write.{EventJournalWriter, SnapshotJournalWriter}
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.language.unsafeNulls
import scala.util.control.NonFatal

/** Writes events to journal file and awaits acknowledgment from passive cluster node.
  *
  */
final class Journaler[S <: SnapshotableState[S]] private(
  recovered: Recovered[S],
  journalingObserver: JournalingObserver,
  val conf: JournalConf,
  clock: WallClock,
  eventIdGenerator: EventIdGenerator,
  queue: Queue[IO, Option[QueueEntry[S]]],
  ackSignal: SignallingRef[IO, EventId],
  requireClusterAck: SignallingRef[IO, Boolean])
  (using S: SnapshotableState.Companion[S], sTag: Tag[S], ioRuntime: IORuntime)
extends
  Service.StoppableByRequest,
  JournalLogging:

  assert(S eq recovered.journalLocation.S)

  private var lastJournalHeader = recovered.recoveredJournalFile match
    case None => JournalHeader.initial[S](timestamp = clock.now())
    case Some(o) => o.nextJournalHeader
  val journalId: JournalId = lastJournalHeader.journalId
  private val totalRunningSince = recovered.totalRunningSince
  val journalLocation: JournalLocation = recovered.journalLocation

  private val state = AsyncVariable(
    State.initial(recovered.state, lastJournalHeader.totalEventCount),
    suppressLog = true)

  private var eventWriter: EventJournalWriter = null.asInstanceOf

  private val committerFiber = AsyncVariable(none[FiberIO[Unit]])
  private val committerTerminatedUnexpectedly = Deferred.unsafe[IO, Either[Throwable, Unit]]
  private val suspendCommitterLock = AsyncLock()
  private val processingPassiveLost = Atomic(0)

  private var lastSnapshotTakenEventId = EventId.BeforeFirst
  private var releaseEventIdsAfterClusterCoupledAck: Option[EventId] = None
  private var isSwitchedOver = false
  private val statistics = new Statistics

  eventIdGenerator.updateLastEventId(recovered.state.eventId)

  def isHalted: Boolean =
    isSwitchedOver || isStopping

  protected def start =
    IO.defer:
      for o <- conf.simulateSync do logger.warn(s"Disk sync is simulated with a ${o.pretty} pause")
      if lastJournalHeader.eventId == EventId.BeforeFirst then
        logger.info("Starting a new empty journal")
      logger.whenTraceEnabled:
        logger.debug("Logger isTraceEnabled=true")
      assertIsRecoverable(state.get.uncommitted)
      IO.defer:
        val tmpFile = JournalLocation.toTemporaryFile:
          journalLocation.file(after = state.get.committed.eventId)
        if exists(tmpFile) then
          logger.warn(s"JournalWriter: Deleting existent file '$tmpFile'")
          delete(tmpFile)
        takeSnapshot(isStarting = true)
      *>
        startService:
          val untilCommitterFailed = committerTerminatedUnexpectedly.get.flatMap:
            case Left(t) => IO.raiseError(t)
            case Right(()) => IO.raiseError:
              new RuntimeException("Journaler committer terminated unexpectedly")
          startCommitter *>
            IO.race(untilCommitterFailed, untilStopRequested)
              .map(_.merge)
              .guarantee:
                stopCommitter.void
              .productR:
                IO.blocking:
                  closeEventWriter()
              .guarantee(IO:
                statistics.logLine.foreach:
                  logger.debug(_))

  def persist[E <: Event](keyedEvents: KeyedEvent[E]*): IO[Checked[Persisted[S, E]]] =
    persist_(Persist(EventCalc.add(keyedEvents)))

  def persist[E <: Event](
    eventCalc: EventCalc[S, E, TimeCtx],
    commitOptions: CommitOptions = CommitOptions.default,
    since: Deadline = Deadline.now)
  : IO[Checked[Persisted[S, E]]] =
    persist(Persist(eventCalc, commitOptions, since))

  inline def persist[E <: Event](persist: Persist[S, E]): IO[Checked[Persisted[S, E]]] =
    persist_(persist)

  private def persist_[E <: Event](persist: Persist[S, E]): IO[Checked[Persisted[S, E]]] =
    enqueue(persist)
      .flatMap: (_, whenPersisted) =>
        whenPersisted.get

  // TODO Visible only for legacy JournalActor.
  private[journal] def enqueue[E <: Event](persist: Persist[S, E])
  : IO[(DeferredSource[IO, Checked[Persisted[S, E]]], DeferredSource[IO, Checked[Persisted[S, E]]])] =
    // TODO
    // WHEN STOPPED WHILE SWITCHING OVER:
    // We ignore the event and do not notify the caller,
    // because it would crash and disturb the process of switching-over.
    // (so AgentDriver with AgentReady event)
    IO.defer:
      val whenApplied = Deferred.unsafe[IO, Checked[Persisted[S, Event]]] // TODO Required only for JournalActor
      val whenPersisted = Deferred.unsafe[IO, Checked[Persisted[S, Event]]]
      val queueEntry = QueueEntry[S](
        persist.eventCalc.widen[S, Event, TimeCtx],
        persist.options, persist.since,
        whenApplied, whenPersisted)
      requireNotStopping *>
        queue.offer(Some(queueEntry))
      .as:
        (whenApplied.asInstanceOf[DeferredSource[IO, Checked[Persisted[S, E]]]],
          whenPersisted.asInstanceOf[DeferredSource[IO, Checked[Persisted[S, E]]]])

  private def committer: IO[Unit] =
    logger.debugIO:
      IO.uncancelable: _ =>
        fs2.Stream.fromQueueNoneTerminated(queue, conf.persistLimit)
          .chunks
          .flatMap: chunk =>
            fs2.Stream.suspend:
              (!isSwitchedOver !! (ClusterNodeHasBeenSwitchedOverProblem: Problem)) >>
                checkNotStopping
              match
                case Left(problem) =>
                  fs2.Stream.exec:
                    chunk.traverse: queueEntry =>
                      queueEntry.whenApplied.complete(Left(problem)) *>
                        queueEntry.whenPersisted.complete(Left(problem))
                    .void
                case Right(()) =>
                  fs2.Stream.emit(chunk)
          .evalMap[IO, Chunk[Applied]]/*IntelliJ*/: chunk =>
            // Try to keep the chunks for faster processing!
            chunk.flatTraverse: queueEntry =>
              state.updateCheckedWithResult: state =>
                IO:
                  applyEvents(state, queueEntry).map: (aggregate, n, applied) =>
                    val updatedState = state.copy(
                      uncommitted = aggregate,
                      totalEventCount = state.totalEventCount + n)
                    (updatedState, applied)
              .flatMap:
                case Left(problem) =>
                  queueEntry.whenApplied.complete(Left(problem)) *>
                    queueEntry.whenPersisted.complete(Left(problem))
                      .as(Chunk.empty)
                case Right(applied) =>
                  applied.completeApplied
                    .productR:
                      IO.whenA(applied.commitOptions.commitLater):
                        applied.completePersisted
                    .productR:
                      applied.stampedKeyedEvents.lastOption.map(_.value.event) match
                        case Some(_: ClusterSwitchedOver) =>
                          IO:
                            logger.debug("ClusterSwitchedOver: no more events are accepted")
                            isSwitchedOver = true
                        case Some(_: ClusterPassiveLost) =>
                          logger.debug("ClusterPassiveLost: acknowledgements are no longer required")
                          processingPassiveLost += 1
                          requireClusterAck.set(false)
                        case _ => IO.unit
                    .as:
                      Chunk.singleton(applied)
          // TODO use (commitOptions.delay max conf.delay) - options.alreadyDelayed
          //<editor-fold desc="// ...">
          //.chunks
          //.flatTap: chunk =>
          //  fs2.Stream.sleep:
          //    chunk.iterator.map: applied =>
          //      (applied.commitOptions.delay max conf.delay) - applied.commitOptions.alreadyDelayed
          //    .maxOption.getOrElse(ZeroDuration)
          //</editor-fold>
          .filter(_.nonEmpty)
          .prefetch // We have two chunks in the stream
          .evalMap: chunk =>
            chunk.traverse:
              writeToFile
              // OrderStdoutEvents are removed from Written
          .prefetch // We have three chunks in the stream
          .evalMap:
            flushToFile
          .prefetch // We have four chunks in stream //
          .evalTap: chunk =>
            chunk.traverse: written =>
              written.lastStamped.fold(IO.pure(false)): lastStamped =>
                waitForAck(eventId = written.eventId, lastStamped = Some(lastStamped))
              .flatMap: isAcknowledged =>
                state.updateDirect:
                  _.copy(committed = written.aggregate)
                .as:
                  written.copy(isAcknowledged = isAcknowledged)
            .flatTap: chunk =>
              IO:
                journalLogger.logCommitted(chunk.asSeq.view)
                chunk.foreach: written =>
                  statistics.onPersisted(written.eventCount, written.since)
                  eventWriter.onCommitted(written.flushedPositionAndEventId, n = written.eventCount)
              .productR:
                chunk.traverse: written =>
                  IO.unlessA(written.commitOptions.commitLater):
                    written.completePersisted
          .map: chunk =>
            // flushed.stampedKeyedEvents are dropped when commitOptions.commitLater
            chunk.flatMap(o => Chunk.from(o.stampedKeyedEvents))
          .unchunks
          .evalMap:
            handleCommittedJournalOrClusterEvent
          .compile.drain

  private def applyEvents(state: State[S], queueEntry: QueueEntry[S]): Checked[(S, Int, Applied)] =
    catchNonFatalFlatten:
      queueEntry.eventCalc.calculate(state.uncommitted, TimeCtx(clock.now()))
    .map: coll =>
      assertIsRecoverable(state.uncommitted, coll.keyedEvents)
      val stamped = coll.timestampedKeyedEvents.map: o =>
        eventIdGenerator.stamp(o.keyedEvent, timestampMillis = o.maybeMillisSinceEpoch)
      val eventId = stamped.lastOption.fold(coll.aggregate.eventId)(_.eventId)
      val aggregate = coll.aggregate.withEventId(eventId)
      val applied = Applied(
        stamped, aggregate, eventNumber = state.totalEventCount + 1,
        queueEntry.commitOptions, queueEntry.since, queueEntry.whenApplied, queueEntry.whenPersisted)
      (aggregate, stamped.size, applied)

  private def writeToFile(applied: Applied): IO[Written] =
    IO.blocking:
      import applied.{aggregate, commitOptions, eventNumber, since, stampedKeyedEvents, whenPersisted}
      // TODO parallelize serialization properly!
      eventWriter.writeEvents(stampedKeyedEvents, transaction = commitOptions.transaction)
      val lastStamped = stampedKeyedEvents.lastOption
      Written(
        eventId = lastStamped.fold(aggregate.eventId)(_.eventId),
        eventCount = stampedKeyedEvents.size,
        lastStamped,
        if applied.commitOptions.commitLater then
          // Reduce heap usage. The (OrderStdWritten) events will neither be logged nor returned.
          Vector.empty
        else
          stampedKeyedEvents,
        aggregate, since, eventNumber, commitOptions,
        eventWriter.fileLengthAndEventId, whenPersisted)

  private def flushToFile(chunk: Chunk[Written]): IO[Chunk[Written]] =
    IO.blocking:
      eventWriter.flush(sync = conf.syncOnCommit)
      chunk.dropRight(1) ++
        Chunk.fromOption(chunk.last.map(_.copy(isLastOfFlushedOrSynced = true)))

  /** Wait for acknowledgement of passive cluster node.
    *
    * On ClusterPassiveLost terminate with false. */
  private def waitForAck(eventId: EventId, lastStamped: Option[Stamped[AnyKeyedEvent]])
  : IO[Boolean] =
    requireClusterAck.get.flatMap: requireAck =>
      if !requireAck then
        IO.pure(false)
      else
        IO.race(
          // Cancel waiting on ClusterPassiveLost
          requireClusterAck.waitUntil(!_) *> IO:
            logger.debug("requireClusterAck has become false, now cancel waiting for acknowledgement")
            false,
          IO.defer:
            val sym = new BlockingSymbol
            //val n = eventWriter.uncommittedEventCount <-- das sind zu viele Events!
            val since = Deadline.now
            ackSignal.waitUntil(_ >= eventId)
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
                      s" ${stamped.value.event.toShortString}"
                    logger.warn(s"$sym Waiting for ${elapsed.pretty
                      } for acknowledgement from passive cluster node of events until $eventId$event, lastAcknowledgedEventId=${
                      EventId.toString(lastAck)}")
              .productR:
                IO.whenA(sym.warnLogged):
                  IO:
                    logger.info(s"🟢 Events until $eventId have finally been acknowledged after ${
                      since.elapsed.pretty}")
                    sym.clear()
              .as(true)
        ).map(_.merge)

  private def handleCommittedJournalOrClusterEvent(stamped: Stamped[AnyKeyedEvent]): IO[Unit] =
    stamped.value.event match
      case SnapshotTaken | _: JournalEventsReleased =>
        // SnapshotTaken does not run through the committer ???
        releaseObsoleteEvents

      case _: ClusterCoupled =>
        IO.defer:
          if processingPassiveLost.get() > 0 then
            IO:
              logger.warn("ClusterCoupled followed by ClusterPassiveLost")
          else
            logger.info:
              "Cluster is coupled: Start requiring acknowledgements from passive cluster node"
            requireClusterAck.set(true)
              .productR(IO:
                releaseEventIdsAfterClusterCoupledAck = Some(stamped.eventId))

      case _: ClusterFailedOver | _: ClusterActiveNodeShutDown | ClusterResetStarted =>
        requireClusterAck.set(false)

      case _: ClusterPassiveLost =>
        IO:
          processingPassiveLost -= 1

      case _ => IO.unit

  private def suspendCommitter: ResourceIO[Unit] =
    suspendCommitterLock.resource.flatTap: _ =>
      Resource.make(stopCommitter)(IO.whenA(_)(startCommitter))

  private def startCommitter: IO[Unit] =
    committerFiber.update:
      case Some(_) =>
        IO.raiseError(throw new IllegalStateException(
          "Journaler#startCommitter despite comitter is already running"))
      case None =>
        logger.trace("startComitter")
        committer.start
          .flatTap: startedFiber =>
            startedFiber.joinStd.attempt.flatMap: terminated =>
              // Fail our service when committer terminates before stopCommitter
              committerFiber.use: currentFiber =>
                val beforeStopComitter = currentFiber.exists(_ eq startedFiber)
                if beforeStopComitter then
                  logger.debug:
                    s"💥 Committer terminated with ${terminated.merge} before stopCommitter"
                else
                  logger.trace("Committer terminated after stopCommitter ✔")
                IO.whenA(beforeStopComitter):
                  committerTerminatedUnexpectedly.complete(terminated).void
            .start // No need to join
          .map(Some(_))
    .void

  private def stopCommitter: IO[Boolean] =
    committerFiber.updateWithResult:
      case None => IO.pure(None -> false)
      case Some(fiber) =>
        logger.trace("stopCommitter")
        queue.offer(None) *>
          fiber.joinStd.as(None -> true)

  def onPassiveNodeHasAcknowledged(eventId: EventId): IO[Unit] =
    ackSignal.update: lastAck =>
      if eventId < lastAck then
        logger.warn(s"Passive cluster node acknowledgded old $eventId EventId after $lastAck")
        lastAck
      else
        val lastEventId = state.get.uncommitted.eventId
        if isSwitchedOver && lastEventId < eventId then
          // The other cluster node may already have become active (uncoupled),
          // generating new EventIds whose last one we may receive here.
          // So we take the last one we know (must be the EventId of ClusterSwitchedOver)
          // TODO Can web service /api/journal suppress EventIds on passive node side after becoming active?
          lazy val msg = s"Passive cluster node isAcknowledged future event ${EventId.toString(eventId)}" +
            s" while lastEventId=${EventId.toString(lastEventId)} (okay when switching over)"
          if lastAck < lastEventId then logger.warn(msg) else logger.debug(s"❓ $msg")
          lastEventId
        else
          eventId
    .productR:
      IO.whenA(releaseEventIdsAfterClusterCoupledAck.isDefined):
        releaseObsoleteEvents

  def takeSnapshot(isStarting: Boolean = false): IO[Unit] =
    logger.debugIO("takeSnapshot", isStarting ?? "isStarting"):
      suspendCommitter.surround:
        state.update: state =>
          takeSnapshot2(state, isStarting = isStarting)
      .productR:
        releaseObsoleteEvents

  private def takeSnapshot2(state: State[S], isStarting: Boolean): IO[State[S]] =
    IO.defer:
      val since = Deadline.now
      assertThat(state.uncommitted eq state.committed)

      if eventWriter != null then
        closeEventWriter()

      val snapshotTaken = eventIdGenerator.stamp(NoKey <-: JournalEvent.SnapshotTaken)
      val aggregate = state.committed.applyStampedEvents(snapshotTaken :: Nil).orThrow

      val file = journalLocation.file(after = state.committed.eventId)
      val journalHeader = lastJournalHeader.nextGeneration[S](
        eventId = state.committed.eventId,
        totalEventCount = state.totalEventCount,
        totalRunningTime = totalRunningSince.elapsed.roundUpToNext(1.ms),
        timestamp = clock.now())
      logger.info(s"Starting new journal file #${journalHeader.generation} ${file.getFileName
        } with a snapshot ${if conf.syncOnCommit then "(using sync)" else "(no sync)"}")
      logger.debug(journalHeader.toString)

      val checkingRecoverer = conf.slowCheckState ? S.newRecoverer()
      for
        PositionAnd(fileLengthBeforeEvents, fileEventId) <-
          SnapshotJournalWriter.writeSnapshotStream(
            S, file, journalHeader,
            state.committed.toSnapshotStream.map: o =>
              checkingRecoverer.foreach(_.addSnapshotObject(o))
              o,
            snapshotTaken,
            syncOnCommit = conf.syncOnCommit,
            simulateSync = conf.simulateSync)
        _ <- IO:
          checkingRecoverer.foreach: recoverer => // Simulate recovery
            val recoveredAggregate = recoverer.result().withEventId(state.committed.eventId)
            assertEqualSnapshotState("Written snapshot", state.committed, recoveredAggregate)
        state <- IO.defer:
          lastJournalHeader = journalHeader
          lastSnapshotTakenEventId = snapshotTaken.eventId

          eventWriter =
            newEventJsonWriter(fileEventId = fileEventId, after = snapshotTaken.eventId)
          eventWriter.onJournalingStarted(fileLengthBeforeEvents = fileLengthBeforeEvents)
          locally:
            if isStarting then
              // This is because after starting a Coupled active node,
              // the passive node is still not ready to acknowledged,
              // instead we get a deadlock with endless ClusterRecouple retries
              // TODO Maybe issue an PassiveLost after SnapshotToken
              //  to change to ClusterState.PassiveLost ?
              IO.pure(false)
            else
              waitForAck(aggregate.eventId, Some(snapshotTaken))
          .flatMap: isAcknowledged =>
            IO:
              journalLogger.logCommitted(
                eventNumber = state.totalEventCount,
                snapshotTaken :: Nil,
                isTransaction = false, isAcknowledged = isAcknowledged,
                since)
              eventWriter.onCommitted(eventWriter.fileLengthAndEventId, n = 1)
          .productR:
            IO:
              val how = if conf.syncOnCommit then "(with sync)" else "(without sync)"
              logger.debug(s"Snapshot written $how to journal file ${file.getFileName}")

              state.copy(
                uncommitted = aggregate,
                committed = aggregate,
                totalEventCount = state.totalEventCount + 1)
      yield
        state

  private def newEventJsonWriter(fileEventId: EventId, after: EventId) =
    val w = new EventJournalWriter(journalLocation,
      fileEventId = fileEventId, after = after,
      journalId,
      journalingObserver, simulateSync = conf.simulateSync,
      initialEventCount = 1 /*SnapshotTaken*/)
    journalLocation.updateSymbolicLink(w.file)
    w

  private def closeEventWriter(): Unit =
    if eventWriter != null then
      if isSwitchedOver then
        eventWriter.flush(sync = conf.syncOnCommit)
        eventWriter.close()
      else
        eventWriter.closeProperly(sync = conf.syncOnCommit)
      eventWriter = null

  private def releaseObsoleteEvents: IO[Unit] =
    IO.whenA(conf.deleteObsoleteFiles):
      for
        state <- state.value
        requireAck <- requireClusterAck.get
        lastAck <- ackSignal.get
        _ <- IO:
          val committed = state.committed
          val clusterState = committed.clusterState
          if clusterState == ClusterState.Empty ||
            requireAck
              // ClusterPassiveLost after SnapshotTaken in the same commit chunk has reset
              // requireClusterAck. We must not delete the file when cluster is being decoupled.
              && (clusterState.isInstanceOf[ClusterState.Coupled] ||
              clusterState.isInstanceOf[ClusterState.ActiveShutDown])
              && releaseEventIdsAfterClusterCoupledAck.forall(_ <= lastAck)
          then
            val eventId =
              if clusterState == ClusterState.Empty then
                committed.eventId
              else
                // Do not release the just acknowledged last Event of a journal file
                // (i.e. the last acknowledged event is the last event of a journal file)
                // because after restart, the passive node continues reading the journal file
                // (only to get end-of-file). Subtract 1 to avoid this.
                (lastAck - 1) max EventId.BeforeFirst
            releaseObsoleteEventsUntil:
              committed.journalState.toReleaseEventId(eventId, conf.releaseEventsUserIds)
            releaseEventIdsAfterClusterCoupledAck = None
      yield ()

  private def releaseObsoleteEventsUntil(untilEventId: EventId): Unit =
    val committedState = state.get.committed
    logger.debug(s"releaseObsoleteEvents($untilEventId) ${committedState.journalState}, clusterState=${committedState.clusterState}")
    journalingObserver match
      case JournalingObserver.Dummy =>
        // Without a JournalingObserver, we can delete all previous journal files (for Agent)
        val until = untilEventId min journalHeader.eventId
        for j <- journalLocation.listJournalFiles if j.fileEventId < until do
          val file = j.file
          assertThat(file != eventWriter.file)
          try delete(file)
          catch case NonFatal(t) =>
            logger.warn(s"Cannot delete obsolete journal file '$file': ${t.toStringWithCauses}")
      case o =>
        o.releaseEvents(untilEventId)

  /** Release a concurrent persist operation, which waits for the missing acknowledgement and
    * blocks the persist lock. Avoid a deadlock.
    *
    * MUST BE CALLED BEFORE EMITTING ClusterPassiveLost!
    */
  def onPassiveLost: IO[Unit] =
    IO.defer:
      logger.debug("onPassiveLost: acknowledgements are no longer required")
      requireClusterAck.set(false)

  def unsafeUncommittedAggregate(): S =
    state.get.uncommitted

  def unsafeAggregate(): S =
    state.get.committed

  private[journal] def isRequiringClusterAcknowledgement: IO[Boolean] =
    requireClusterAck.get

  private[journal] def isFlushed: Boolean =
    eventWriter != null && eventWriter.isFlushed

  private[journal] def isSynced: Boolean =
    eventWriter != null && eventWriter.isSynced

  private[journal] def journalHeader: JournalHeader =
    lastJournalHeader

  private def assertIsRecoverable(aggregate: S, keyedEvents: => Seq[AnyKeyedEvent] = Nil)
  : Unit =
    if conf.slowCheckState then
      assertEqualSnapshotState("Recovered", aggregate, aggregate.toRecovered, keyedEvents)

  private def assertEqualSnapshotState(
    what: String,
    aggregate: S,
    couldBeRecoveredState: S,
    keyedEvents: Seq[AnyKeyedEvent] = Nil)
  : Unit =
    if couldBeRecoveredState != aggregate then
      val msg = s"$what does not match actual '$S'"
      logger.error(msg)
      try
        keyedEvents.foreachWithBracket(Round): (keyedEvent, bracket) =>
          logger.error(s"$bracket${keyedEvent.toString.truncateWithEllipsis(200)}")
        logger.error("Snapshot objects: ⏎")
        aggregate.toSnapshotStream.zipWithBracket(Square).map: (o, br) =>
          logger.error(s"$br$o")
        .compile.drain
        //couldBeRecoveredState.toSnapshotStream.zipWithBracket(Round).foreach: (o, br) =>
        //  IO(logger.error(s"$br$o"))
        //.compile.drain
        SnapshotableState.logBoth(
          couldBeRecoveredState, s"$what is WRONG?",
          aggregate, s"$S is EXPECTED?",
          isTest ? Paths.get("logs/snapshot-error.txt"))
      catch case NonFatal(t) =>
        keyedEvents.foreachWithBracket(Round): (keyedEvent, bracket) =>
          logger.error(s"$bracket${keyedEvent.toString.truncateWithEllipsis(200)}")
        aggregate.emitLineStream(logger.error(_))
        aggregate.toSnapshotStream.zipWithBracket(Square).map: (o, br) =>
          logger.error(s"$br$o")
        .compile.drain
        throw t
      throw new AssertionError(msg)

  override def toString = s"Journaler[${journalLocation.S}]" //(${journalLocation.fileBase})"

  sealed trait AppliedOrFlushed:
    val stampedKeyedEvents: Vector[Stamped[AnyKeyedEvent]]
    val aggregate: S
    val commitOptions: CommitOptions
    val whenPersisted: DeferredSink[IO, Checked[Persisted[S, Event]]]

    final def completePersisted: IO[Unit] =
      whenPersisted.complete:
        Right(Persisted(stampedKeyedEvents, aggregate))
      .void

    def traceLog(): Unit =
      val prefix = if this.isInstanceOf[Written] then "✔" else "+"
      stampedKeyedEvents.foreachWithBracket(
        if commitOptions.transaction then Round else Square
      ): (o, br) =>
        logger.trace(s"### $prefix$br$o")


  /** Events have been applied to State#uncommitted. */
  private final case class Applied(
    stampedKeyedEvents: Vector[Stamped[AnyKeyedEvent]],
    aggregate: S,
    eventNumber: Long,
    commitOptions: CommitOptions,
    since: Deadline,
    whenApplied: DeferredSink[IO, Checked[Persisted[S, Event]]],
    whenPersisted: DeferredSink[IO, Checked[Persisted[S, Event]]]
  ) extends AppliedOrFlushed:
    def completeApplied: IO[Unit] =
      whenApplied.complete:
        Right(Persisted(stampedKeyedEvents, aggregate))
      .void


  /** Events have been written and flushed to the journal file. */
  private final case class Written(
    eventId: EventId,
    eventCount: Int,
    lastStamped: Option[Stamped[AnyKeyedEvent]],
    stampedKeyedEvents: Vector[Stamped[AnyKeyedEvent]],
    aggregate: S,
    since: Deadline,
    eventNumber: Long,
    commitOptions: CommitOptions,
    flushedPositionAndEventId: PositionAnd[EventId],
    whenPersisted: DeferredSink[IO, Checked[Persisted[S, Event]]],
    isAcknowledged: Boolean = false,
    isLastOfFlushedOrSynced: Boolean = false)
  extends AppliedOrFlushed with Loggable:
    def isTransaction = commitOptions.transaction
    def stampedSeq = stampedKeyedEvents
    def nonEmpty = lastStamped.nonEmpty


object Journaler:

  private val logger = Logger[this.type]
  private val meterPersist = CallMeter("Journaler")

  def resource[S <: SnapshotableState[S]: {SnapshotableState.Companion, Tag}](
    recovered: Recovered[S],
    conf: JournalConf,
    eventIdGenerator: Option[EventIdGenerator] = None)
    (using IORuntime)
  : ResourceIO[Journaler[S]] =
    Service.resource:
      for
        _ <- IO:
          val file = recovered.journalLocation.temporaryFile(recovered.eventId)
          if exists(file) then
            logger.warn(s"JournalWriter: Deleting existent file '$file'")
            delete(file)
        queue <- Queue.unbounded[IO, Option[QueueEntry[S]]]
        ackSignal <- SignallingRef[IO].of(EventId.BeforeFirst)
        requireClusterAck <- SignallingRef[IO].of:
          recovered.clusterState.isInstanceOf[ClusterState.Coupled]
        clock <- Environment.environmentOr[WallClock](WallClock)
        eventIdGenerator <- eventIdGenerator match
          case None => Environment.environmentOr[EventIdGenerator](EventIdGenerator(clock))
          case Some(o) => IO.pure(o)
      yield
        new Journaler(recovered, recovered.eventWatch, conf, clock, eventIdGenerator,
          queue, ackSignal, requireClusterAck)


  private case class State[S <: SnapshotableState[S]](
    uncommitted: S,
    committed: S,
    totalEventCount: Long)

  private object State:
    def initial[S <: SnapshotableState[S]](aggregate: S, totalEventCount: Long): State[S] =
      State(aggregate, aggregate, totalEventCount)


  final case class Persist[S <: EventDrivenState[S, E], E <: Event](
    eventCalc: EventCalc[S, E, TimeCtx],
    options: CommitOptions = CommitOptions.default,
    since: Deadline = Deadline.now)


  final case class Persisted[S <: EventDrivenState[S, E], E <: Event](
    stampedKeyedEvents: IndexedSeq[Stamped[KeyedEvent[E]]],
    aggregate: S)


  private final case class QueueEntry[S <: EventDrivenState[S, Event]](
    eventCalc: EventCalc[S, Event, TimeCtx],
    commitOptions: CommitOptions = CommitOptions.default,
    since: Deadline,
    whenApplied: DeferredSink[IO, Checked[Persisted[S, Event]]],
    whenPersisted: DeferredSink[IO, Checked[Persisted[S, Event]]])


  private class Statistics:
    // Use CallMeter for this???
    private var eventCount = 0L
    private var persistCount = 0L
    private var persistDurationMin = FiniteDuration.MaxValue.toMillis
    private var persistDurationMax = 0L
    private var persistDurationSum = 0L

    def onPersisted(eventCount: Int, since: Deadline): Unit =
      this.eventCount += eventCount
      persistCount += 1
      val duration = since.elapsed.toNanos
      persistDurationSum += duration
      if persistDurationMin > duration then persistDurationMin = duration
      if persistDurationMax < duration then persistDurationMax = duration

    def logLine: Option[String] =
      persistCount > 0 thenSome:
        val persist = 1.0 * eventCount / persistCount
        val min = persistDurationMin.ns.pretty
        val max = persistDurationMax.ns.pretty
        val avg = persistDurationAvg.pretty
        f"$persistCount persists · $eventCount events ($persist%.1f/persist) $min…∅$avg…$max"

    private def persistDurationAvg = (persistDurationSum / persistCount).ns

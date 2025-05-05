package js7.journal

import cats.effect
import cats.effect.kernel.DeferredSink
import cats.effect.{Deferred, IO, Outcome}
import cats.syntax.flatMap.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.Chunk
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.startAndForget
import js7.base.catsutils.CatsEffectUtils.outcomeToEither
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.metering.CallMeter
import js7.base.monixutils.AsyncVariable
import js7.base.problem.Checked.catchNonFatalFlatten
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.CatsUtils.syntax.{logWhenItTakesLonger, whenItTakesLonger}
import js7.base.utils.MultipleLinesBracket.{Round, Square}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, Atomic, ByteUnits, MultipleLinesBracket}
import js7.common.jsonseq.PositionAnd
import js7.data.Problems.ClusterNodeHasBeenSwitchedOverProblem
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost, ClusterResetStarted, ClusterSwitchedOver}
import js7.data.cluster.ClusterState
import js7.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import js7.data.event.TimestampedKeyedEvent.{keyedEvent, maybeMillisSinceEpoch}
import js7.data.event.{AnyKeyedEvent, Event, EventId, JournalEvent, KeyedEvent, SnapshotableState, Stamped, TimeCtx}
import js7.journal.Committer.*
import js7.journal.FileJournal.*
import js7.journal.Journal.Persisted
import js7.journal.log.JournalLogger
import js7.journal.log.JournalLogger.Loggable
import js7.journal.write.EventJournalWriter
import scala.concurrent.duration.Deadline
import scala.language.unsafeNulls

transparent trait Committer[S <: SnapshotableState[S]]:
  journal: FileJournal[S] =>

  private val snapshotLock = AsyncLock()
  private val currentService = AsyncVariable(none[CommitterService])
  private var _isSwitchedOver = false
  private val committerTerminatedUnexpectedly = Deferred.unsafe[IO, Either[Throwable, Unit]]
  private val journalLogger = JournalLogger(conf)
  private val statistics = new Statistics

  protected final def logStatistics(): Unit =
    statistics.logLine.foreach:
      logger.debug(_)

  protected final def whenCommitterTerminatedUnexpectedly: IO[Either[Throwable, Unit]] =
    committerTerminatedUnexpectedly.get

  protected final def isSwitchedOver =
    _isSwitchedOver

  protected final def snapshotPeriodically: IO[Unit] =
    fs2.Stream.awakeEvery[IO](conf.snapshotPeriod)
      .interruptWhenF(untilStopRequested)
      .evalMap: _ =>
        takeSnapshot
      .compile.drain

  final def takeSnapshot: IO[Unit] =
    takeSnapshot(ignoreIsStopping = false)

  protected final def takeSnapshot(ignoreIsStopping: Boolean): IO[Unit] =
    // A snapshot is taken through stopping and starting the Committer.
    logger.debugIO("takeSnapshot"):
      snapshotLock.lock:
        IO.defer:
          // The new snapshot's EventId must differ from the last snapshot's EventId,
          // otherwise no snapshot is taken, and the committer continues.
          IO.whenA(
            (!isStopping || ignoreIsStopping) &&
              state.get.committed.eventId > lastSnapshotTakenEventId
          ):
            IO.uncancelable: _ => // Uncancelable !!!
              stopCommitter >>
                IO.whenA(!isStopping || ignoreIsStopping):
                  startCommitter(isStarting = false)
            .logWhenItTakesLonger

  protected final def startCommitter(isStarting: Boolean): IO[Unit] =
    currentService.update:
      case Some(_) =>
        IO.raiseError(throw new IllegalStateException(
          "startCommitter but committer is already running"))
      case None =>
        logger.traceIO("startCommitter"):
          startNewJournalFile(isStarting = isStarting).allocated.flatMap: (eventWriter, close) =>
            Service.resource(IO(new CommitterService(eventWriter)))
              .allocated.flatMap: (committerService, stop) =>
                committerService.untilStopped
                  .guarantee:
                    close
                  .guaranteeCase: outcome =>
                    IO.defer:
                      IO.unlessA(committerService.isStopping):
                        whenKilling.tryGet.map(_.isDefined).flatMap: isKilling =>
                          val terminated = outcomeToEither(outcome).rightAs(())
                          // Warning might be duplicate with Service warnings ?
                          outcome match
                            case Outcome.Succeeded(_) =>
                              if !isKilling then logger.warn("Committer terminated unexpectedly")
                            case Outcome.Errored(t) =>
                              logger.warn(s"Committer terminated unexpectedly with ${t.toStringWithCauses}")
                            case Outcome.Canceled() =>
                              logger.warn("Committer has been cancelled")
                            IO.whenA(!isKilling):
                              committerTerminatedUnexpectedly.complete(terminated).void
                  .start
                  .as(Some(committerService))
          <*
            releaseObsoleteEvents
    .void

  protected final def stopCommitter: IO[Unit] =
    currentService.update:
      case None => IO.pure(None)
      case Some(committerService) =>
        committerService.stop
          .logWhenItTakesLonger("committerService.stop")
          .as(None)
    .void

  protected final def onSnapshotTaken(
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
        IO.pure(false)
      else
        waitForAck(snapshotTaken.eventId, Some(snapshotTaken))
    .flatMap: isAcknowledged =>
      IO:
        statistics.onPersisted(eventCount = 1, since)
        journalLogger.logCommitted(snapshotTaken :: Nil,
          eventNumber = eventNumber, since, isAcknowledged = isAcknowledged)
        eventWriter.onCommitted(eventWriter.fileLengthAndEventId, n = 1)

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

  private def noMoreAcks(reason: String): IO[Unit] =
    requireClusterAck.getAndUpdate(_ => false).map: wasAck =>
      IO:
        if wasAck then
          logger.info(s"Cluster passive node acknowledgements are no longer awaited due to $reason")

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
          logWaitingForAck(eventId, lastStamped):
            ackSignal.waitUntil(_ >= eventId)
              .as(true)
        ).map(_.merge)

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
                } for acknowledgement from passive cluster node of events until ${
                eventId}$event, last acknowledged: ${EventId.toString(lastAck)}")
        .productL:
          IO.whenA(sym.warnLogged):
            IO:
              logger.info(s"🟢 Events until $eventId have finally been acknowledged after ${
                since.elapsed.pretty}")
              sym.clear()


  /** The CommitterService writes a single journal file, starting with a snapshot. */
  private final class CommitterService(eventWriter: EventJournalWriter)
  extends Service.StoppableByRequest:
    private val processingPassiveLost = Atomic(0)
    private val snapshotBecauseBig = Atomic(false)

    override def toString = "CommitterService"

    def start =
      startService:
        IO.uncancelable: _ =>
          (untilStopRequested *> journalQueue.offer(None)).background.surround:
            runStream

    override def stop =
      super.stop

    private def runStream: IO[Unit] =
      fs2.Stream.fromQueueNoneTerminated(journalQueue, conf.persistLimit)
        .through(pipe)
        .interruptWhenF(whenKilling.get)
        .compile.drain

    private def pipe: fs2.Pipe[IO, QueueEntry[S], Unit] = _
      .chunks
      .evalMap[IO, Chunk[Applied]]/*IntelliJ*/: chunk =>
        // Try to preserve the chunks for faster processing!
        chunk.flatTraverse: queueEntry =>
          state.updateCheckedWithResult: state =>
            IO:
              (!_isSwitchedOver !! (ClusterNodeHasBeenSwitchedOverProblem: Problem)) >>
                //?journal.checkNotStopping >>
                applyEvents(state, queueEntry).map: (aggregate, applied) =>
                  state.copy(
                    uncommitted = aggregate,
                    totalEventCount = state.totalEventCount + applied.stampedKeyedEvents.size
                  ) -> applied
          .flatMap:
            case Left(problem) =>
              queueEntry.whenApplied.complete(Left(problem)) *>
                queueEntry.whenPersisted.complete(Left(problem))
                  .as(Chunk.empty)
            case Right(applied: Applied/*IntelliJ*/) =>
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
                  .productR:
                    IO.whenA(applied.commitOptions.commitLater):
                      applied.completePersisted
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
      .prefetch // Process two chunks concurrently //
      .evalMap:
        writeToFile
        // maybe optimize: flush concurrently, but only whole lines must be flushed
      // OrderStdoutEvents are removed from Written
      .prefetch // Process three chunks concurrently //
      .evalMap: chunk =>
        // TODO Complete a single Written as soon as it has been acknowledged
        // - Collect all Written that have been acknowledged at once
        // - But the original chunk should be logCommitted (split logCommitted?)
        val lastWritten = chunk.last.get // chunk is nonEmpty
        val clusterState = lastWritten.aggregate.clusterState
        IO.unlessA(clusterState.isInstanceOf[ClusterState.Coupled]):
          // Do not switch requireClusterAck on, because
          // TODO ClusterResetStarted doesn't leave the Coupled state
          noMoreAcks(clusterState.getClass.getSimpleName)
        .productR:
          waitForAck(eventId = lastWritten.eventId, lastStamped = lastWritten.lastStamped)
        .map:
          if _ then
            chunk.map: written =>
              written.copy(isAcknowledged = true)
          else
            chunk
        .flatTap: _ =>
          state.updateDirect:
            _.copy(committed = lastWritten.aggregate)
      .evalTap: chunk =>
        journalLogger.logCommitted(chunk.asSeq.view)
        chunk.traverse: written =>
          IO.defer:
            statistics.onPersisted(written.eventCount, written.since)
            eventWriter.onCommitted(written.positionAndEventId, n = written.eventCount)
            IO.unlessA(written.commitOptions.commitLater):
              written.completePersisted
      .evalTap: chunk =>
        // flushed.stampedKeyedEvents have been dropped when commitOptions.commitLater
        chunk.flatMap(o => Chunk.from(o.stampedKeyedEvents)).traverse: stamped =>
          possiblySwitchAck(stamped)
      .evalTap: chunk =>
        IO.whenA(shouldReleaseObsoleteEvents(chunk)):
          releaseObsoleteEvents
      .evalMap: _ =>
        maybeDoASnasphot

    private def applyEvents(state: State[S], queueEntry: QueueEntry[S]): Checked[(S, Applied)] =
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
        (aggregate, applied)

    private def writeToFile(chunk: Chunk[Applied]): IO[Chunk[Written]] =
      IO.blocking:
        // TODO parallelize JSON serialization properly!
        chunk.map: applied =>
          import applied.{commitOptions, eventNumber, since, stampedKeyedEvents, whenPersisted}
          val positionAndEventId =
            eventWriter.writeEvents(stampedKeyedEvents, transaction = commitOptions.transaction)
          val lastStamped = stampedKeyedEvents.lastOption
          Written(
            eventId = lastStamped.fold(applied.aggregate.eventId)(_.eventId),
            eventCount = stampedKeyedEvents.size,
            lastStamped,
            if applied.commitOptions.commitLater then
              // Reduce heap usage. The (OrderStdWritten) events will neither be logged nor returned.
              Vector.empty
            else
              stampedKeyedEvents,
            applied.aggregate, since, eventNumber, commitOptions, whenPersisted,
            positionAndEventId)
      .flatMap: chunk =>
        IO.blocking:
          eventWriter.flush(sync = conf.syncOnCommit)
          markAsFlushed(chunk)

    /** For logging: mark the last non-empty chunk as isLastOfFlushedOrSynced. */
    private def markAsFlushed(chunk: Chunk[Written]): Chunk[Written] =
      var i = chunk.size
      if chunk.reverseIterator.exists: written =>
        i -= 1
        written.stampedKeyedEvents.nonEmpty // An existing event (not deleted due to commitLater)
      then
        val (a, b) = chunk.splitAt(i)
        a ++ b.map(_.copy(isLastOfFlushedOrSynced = true))
      else
        chunk

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
            logger.debug(s"Take snapshot because written size ${toKBGB(eventWriter.bytesWritten)
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


  private sealed trait AppliedOrFlushed:
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
    whenPersisted: DeferredSink[IO, Checked[Persisted[S, Event]]],
    positionAndEventId: PositionAnd[EventId],
    isAcknowledged: Boolean = false,
    isLastOfFlushedOrSynced: Boolean = false)
  extends AppliedOrFlushed with Loggable:
    def isTransaction = commitOptions.transaction
    def stampedSeq = stampedKeyedEvents
    def nonEmpty = eventCount > 0

end Committer


object Committer:
  private val logger = Logger[this.type]
  private val meterEstimatedSnapshotSize = CallMeter("FileJournal.estimatedSnapshotSize")

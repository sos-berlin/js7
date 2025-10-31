package js7.journal

import cats.effect.kernel.{DeferredSink, DeferredSource}
import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref, ResourceIO}
import cats.syntax.flatMap.*
import fs2.concurrent.SignallingRef
import izumi.reflect.Tag
import java.nio.file.Files.{delete, exists}
import java.nio.file.Path
import js7.base.catsutils.CatsEffectExtensions.{left, right}
import js7.base.catsutils.Environment
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.metering.CallMeter
import js7.base.monixutils.{AsyncVariable, Switch}
import js7.base.problem.{Checked, Problem}
import js7.base.service.Problems.ServiceStoppedProblem
import js7.base.service.Service
import js7.base.system.MBeanUtils.registerMBean
import js7.base.time.ScalaTime.*
import js7.base.time.{Timestamp, WallClock}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.logWhenMethodTakesLonger
import js7.base.utils.MultipleLinesBracket.{Round, Square, zipWithBracket}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.data.cluster.ClusterState
import js7.data.event.{AnyKeyedEvent, Event, EventCalc, EventDrivenState, EventId, JournalHeader, JournalId, JournalState, SnapshotableState, TimeCtx}
import js7.journal.FileJournal.*
import js7.journal.FileJournalMXBean.Bean
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.extensions.*
import js7.journal.problems.Problems.JournalKilledProblem
import js7.journal.recover.Recovered
import js7.journal.watch.{JournalEventWatch, JournalingObserver}
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.language.unsafeNulls
import scala.util.control.NonFatal

/** Writes events in an anytime recoverable way to a journal.
  * <ul>
  *   <li>
  *     Awaits acknowledgment from a passive cluster node
  * </ul>
  */
final class FileJournal[S <: SnapshotableState[S]: Tag] private(
  recovered: Recovered[S],
  val conf: JournalConf,
  protected val journalQueue: Queue[IO, Option[QueueEntry[S]]],
  protected val restartSnapshotTimerSignal: SignallingRef[IO, Unit],
  protected val requireClusterAck: SignallingRef[IO, Boolean],
  protected val ackSignal: SignallingRef[IO, EventId],
  protected val clock: WallClock,
  protected val eventIdGenerator: EventIdGenerator,
  protected val bean: FileJournalMXBean.Bean)
  (using
    protected val S: SnapshotableState.Companion[S])
extends
  Service.StoppableByRequest,
  Committer[S],
  Snapshotter[S],
  Journal[S],
  FileJournal.PossibleFailover:

  assert(recovered.journalLocation.S eq S)

  private val totalRunningSince = recovered.totalRunningSince
  val journalLocation: JournalLocation = recovered.journalLocation
  val eventWatch: JournalEventWatch = recovered.eventWatch

  protected val journalingObserver: JournalingObserver = recovered.eventWatch

  protected var lastJournalHeader: JournalHeader =
    recovered.recoveredJournalFile match
      case None => JournalHeader.initial[S](timestamp = clock.now())
      case Some(o) => o.nextJournalHeader

  protected val state = AsyncVariable(
    State.initial(recovered.state, lastJournalHeader.totalEventCount),
    suppressLog = true)

  val journalId: JournalId = lastJournalHeader.journalId

  private val _deleteJournalWhenStopping = Ref.unsafe[IO, Boolean](false)
  protected val whenBeingKilled = Deferred.unsafe[IO, Unit]
  @volatile protected var isBeingKilled = false
  protected var releaseEventIdsAfterClusterCoupledAck: Option[EventId] = None
  protected var _suppressSnapshotWhenStopping = false

  eventIdGenerator.updateLastEventId(state.get.uncommitted.eventId)

  def isHalted: Boolean =
    isSwitchedOver || isStopping

  def totalRunningTime: FiniteDuration =
    totalRunningSince.elapsed

  def initiallyStartedAt: Timestamp =
    lastJournalHeader.initiallyStartedAt

  /** For testing an abrupt program termination. */
  def suppressSnapshotWhenStopping(): Unit =
    _suppressSnapshotWhenStopping = true

  protected def start =
    IO.defer:
      bean.totalOperatingTimeUntilStart := lastJournalHeader.totalRunningTime
      for o <- conf.simulateSync do logger.warn(s"Disk sync is simulated with a ${o.pretty} pause")
      if lastJournalHeader.eventId == EventId.BeforeFirst then
        logger.info("Starting a new empty journal")
      logger.whenTraceEnabled:
        logger.debug("Logger isTraceEnabled=true")
      if conf.slowCheckState then
        assertIsRecoverable(state.get.uncommitted)

      val tmpFile = JournalLocation.toTemporaryFile:
        journalLocation.file(after = state.get.committed.eventId)
      if exists(tmpFile) then
        logger.warn(s"JournalWriter: Deleting existent file '$tmpFile'")
        delete(tmpFile)

      startCommitter(isStarting = true) *>
        startService:
          snapshotPeriodically.background.surround:
            IO.race(untilStopRequested, whenCommitterTerminatedUnexpectedly)
              .flatMap:
                case Left(()) => IO.unit
                case Right(Left(t)) => IO.raiseError(t)
                case Right(Right(())) => IO.raiseError:
                  new RuntimeException("Journal committer terminated unexpectedly")
              .guarantee:
                IO.defer:
                  IO.whenA(!isSwitchedOver && !_suppressSnapshotWhenStopping):
                    takeSnapshot(ignoreIsStopping = true, dontSignal = ())
              .guarantee:
                stopCommitter
              .guarantee:
                rejectQueuedEntries
              .guarantee:
                _deleteJournalWhenStopping.get.flatMap(IO.whenA(_):
                  IO.blocking:
                    journalLocation.deleteJournal(ignoreFailure = true))
              .guarantee(IO:
                logStatistics())

  private def rejectQueuedEntries: IO[Unit] =
    IO.defer:
      val problem = Left(if isBeingKilled then JournalKilledProblem else ServiceStoppedProblem(toString))
      ().tailRecM: _ =>
        journalQueue.tryTake.flatMap:
          case None => IO.right(())
          case Some(None) => IO.left(())
          case Some(Some(queuedEntry)) =>
            queuedEntry.whenApplied.complete(problem) *>
              queuedEntry.whenPersisted.complete(problem)
                .as(Left(()))

  // TODO Prefer proper initialization and termination order
  override def stop: IO[Unit] =
    super.stop

  /** For testing a failover. */
  def kill: IO[Unit] =
    logger.debugIO:
      IO.defer:
        _suppressSnapshotWhenStopping = true
        isBeingKilled = true
        whenBeingKilled.complete(()) *>
          noMoreAcks("kill") *>
          stop

  def deleteJournalWhenStopping: IO[Unit] =
    _deleteJournalWhenStopping.set(true)

  def prepareForStopOfClusterNodeStop: IO[Unit] =
    noMoreAcks("prepareForStopOfClusterNodeStop")

  protected def noMoreAcks(reason: String): IO[Unit] =
    requireClusterAck.getAndUpdate(_ => false).map: wasAck =>
      IO:
        if wasAck then
          logger.info(s"Cluster passive node acknowledgements are no longer awaited due to $reason")

  protected def persist_[E <: Event](persist: Persist[S, E]): IO[Checked[Persisted[S, E]]] =
    if isTest then assertThat(!persist.commitOptions.commitLater/*not implemented*/)
    //meterPersist:
    enqueue(persist)
      .flatMap: (_, whenPersisted) =>
        whenPersisted.get
      //.raceMerge:
      //  untilStopRequested *>
      //    IO.raiseError(new IllegalStateException("Journal service has been stopped"))
      .logWhenMethodTakesLonger // TODO When cluster hangs then there can be many concurrent log lines

  // TODO Visible only for legacy JournalActor.
  private[journal] def enqueue[E <: Event](persist: Persist[S, E])
  : IO[(DeferredSource[IO, Checked[Persisted[S, E]]], DeferredSource[IO, Checked[Persisted[S, E]]])] =
    // TODO ?
    // WHEN STOPPED WHILE SWITCHING OVER:
    // We ignore the event and do not notify the caller,
    // because it would crash and disturb the process of switching-over.
    // (so AgentDriver with AgentReady event)
    IO.defer:
      val whenApplied = Deferred.unsafe[IO, Checked[Persisted[S, Event]]] // TODO Required only for JournalActor
      val whenPersisted = Deferred.unsafe[IO, Checked[Persisted[S, Event]]]
      val queueEntry = QueueEntry[S](
        persist.eventCalc.widen[S, Event, TimeCtx], persist.commitOptions, persist.since,
        persistMeter.startMetering(),
        whenApplied, whenPersisted)
      IO(!isBeingKilled !! JournalKilledProblem).flatMapT(_ => requireNotStopping).flatMap:
        case Left(problem) =>
          whenApplied.complete(Left(problem)) *> whenPersisted.complete(Left(problem))
        case Right(()) =>
          journalQueue.offer(Some(queueEntry))
      .as:
        whenApplied.asInstanceOf[DeferredSource[IO, Checked[Persisted[S, E]]]] ->
          whenPersisted.asInstanceOf[DeferredSource[IO, Checked[Persisted[S, E]]]]

  def onPassiveNodeHasAcknowledged(eventId: EventId): IO[Unit] =
    ackSignal.update: lastAck =>
      if eventId < lastAck then
        logger.warn(s"Passive cluster node acknowledged old $eventId EventId after $lastAck")
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
      IO.defer:
        IO.whenA(releaseEventIdsAfterClusterCoupledAck.isDefined):
          releaseObsoleteEvents

  protected def releaseObsoleteEvents: IO[Unit] =
    IO.whenA(conf.deleteObsoleteFiles):
      for
        state <- state.value
        requireAck <- requireClusterAck.get
        lastAck <- ackSignal.get
        _ <- IO.defer:
          val committed = state.committed
          val clusterState = committed.clusterState
          IO.whenA(
            clusterState == ClusterState.Empty ||
              requireAck
                // ClusterPassiveLost after SnapshotTaken in the same commit chunk has reset
                // requireClusterAck. We must not delete the file when cluster is being decoupled.
                && (clusterState.isInstanceOf[ClusterState.Coupled] ||
                clusterState.isInstanceOf[ClusterState.ActiveShutDown])
                && releaseEventIdsAfterClusterCoupledAck.forall(_ <= lastAck)
          ):
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
            .productR:
              IO:
                releaseEventIdsAfterClusterCoupledAck = None
      yield ()

  private def releaseObsoleteEventsUntil(untilEventId: EventId): IO[Unit] =
    val committedState = state.get.committed
    logger.debug(s"releaseObsoleteEvents($untilEventId) ${committedState.journalState}, clusterState=${committedState.clusterState}")
    // min lastJournalHeader.eventId, in case it's a JournalObserver.Dummy
    journalingObserver.releaseEvents(untilEventId min lastJournalHeader.eventId)

  def aggregate: IO[S] =
    state.value.map(_.committed)

  def journalState: IO[JournalState] =
    aggregate.map(_.journalState)

  def clusterState: IO[ClusterState] =
    aggregate.map(_.clusterState)

  def unsafeAggregate(): S =
    state.get.committed

  def unsafeUncommittedAggregate(): S =
    state.get.uncommitted

  private[journal] def isRequiringClusterAcknowledgement: IO[Boolean] =
    requireClusterAck.get

  protected def assertIsRecoverable(aggregate: S, keyedEvents: Iterable[AnyKeyedEvent] = Nil): Unit =
    assertEqualSnapshotState("Recovered", aggregate, aggregate.toRecovered, keyedEvents)

  protected def assertEqualSnapshotState(
    what: String,
    aggregate: S,
    couldBeRecoveredState: S,
    keyedEvents: Iterable[AnyKeyedEvent] = Nil)
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
          isTest ? Path.of("logs", "snapshot-error.txt"))
      catch case NonFatal(t) =>
        keyedEvents.foreachWithBracket(Round): (keyedEvent, bracket) =>
          logger.error(s"$bracket${keyedEvent.toString.truncateWithEllipsis(200)}")
        aggregate.emitLineStream(logger.error(_))
        aggregate.toSnapshotStream.zipWithBracket(Square).map: (o, br) =>
          logger.error(s"$br$o")
        .compile.drain
        throw t
      throw new AssertionError(msg)

  override def toString = s"FileJournal[${journalLocation.S}]" //(${journalLocation.fileBase})"


object FileJournal:

  private val logger = Logger[this.type]
  //private val meterPersist = CallMeter("FileJournal.persist")
  private[journal] val persistMeter = CallMeter("FileJournal.persist")

  def service[S <: SnapshotableState[S]: {SnapshotableState.Companion, Tag}](
    recovered: Recovered[S],
    conf: JournalConf,
    eventIdGenerator: Option[EventIdGenerator] = None)
  : ResourceIO[FileJournal[S]] =
    for
      bean <- registerMBean[IO]("Journal", new FileJournalMXBean.Bean)
      journal <- Service.resource:
        for
          queue <- Queue.unbounded[IO, Option[QueueEntry[S]]]
          requireClusterAck <- SignallingRef[IO].of:
            recovered.clusterState.isInstanceOf[ClusterState.Coupled]
          ackSignal <- SignallingRef[IO].of(EventId.BeforeFirst)
          restartSnapshotTimerSignal <- SignallingRef[IO].of(())
          clock <- Environment.environmentOr[WallClock](WallClock)
          eventIdGenerator <- eventIdGenerator match
            case None => Environment.environmentOr[EventIdGenerator](EventIdGenerator(clock))
            case Some(o) => IO.pure(o)
        yield
          FileJournal(recovered, conf, queue, restartSnapshotTimerSignal, requireClusterAck,
            ackSignal, clock, eventIdGenerator, bean)
    yield
      journal

  private[journal] case class State[S <: SnapshotableState[S]](
    uncommitted: S,
    committed: S,
    totalEventCount: Long)

  private[journal] object State:
    def initial[S <: SnapshotableState[S]](aggregate: S, totalEventCount: Long): State[S] =
      State(aggregate, aggregate, totalEventCount)


  private[journal] final case class QueueEntry[S <: EventDrivenState[S, Event]](
    eventCalc: EventCalc[S, Event, TimeCtx],
    commitOptions: CommitOptions,
    since: Deadline,
    metering: CallMeter.Metering,
    whenApplied: DeferredSink[IO, Checked[Persisted[S, Event]]],
    whenPersisted: DeferredSink[IO, Checked[Persisted[S, Event]]]):

    def completePersistedWithProblem(problem: Problem): IO[Unit] =
      IO.defer:
        persistMeter.stopMetering(metering)
        whenApplied.complete(Left(problem)) *>
          whenPersisted.complete(Left(problem)).void

  sealed transparent trait PossibleFailover:
    private val tryingPassiveLostSwitch = Switch(false)

    // Not nestable !!! (or use a readers-writer lock)
    final def forPossibleFailoverByOtherNode[A](io: IO[A]): IO[A] =
      tryingPassiveLostSwitch.switchOnAround(io)

    final val whenNoFailoverByOtherNode: IO[Unit] =
      tryingPassiveLostSwitch.whenOff
        .logWhenMethodTakesLonger


  private[journal] class Statistics:
    // Use CallMeter for this???
    private var eventCount = 0L
    private var persistCount = 0L
    private var persistDurationMin = FiniteDuration.MaxValue.toMillis
    private var persistDurationMax = 0L
    private var persistDurationSum = 0L

    def onPersisted(persistCount: Int, eventCount: Int, since: Deadline): Unit =
      this.eventCount += eventCount
      this.persistCount += persistCount
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

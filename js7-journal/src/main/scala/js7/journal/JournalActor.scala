package js7.journal

import org.apache.pekko.actor.{Actor, ActorRef, DeadLetterSuppression, Props, Stash}
//diffx import com.softwaremill.diffx
import io.circe.syntax.EncoderOps
import java.nio.file.Files.{delete, exists, move}
import java.nio.file.Path
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import js7.base.circeutils.CirceUtils.*
import js7.base.eventbus.EventPublisher
import js7.base.generic.Completed
import js7.base.log.{BlockingSymbol, CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SetOnce
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.common.jsonseq.PositionAnd
import js7.common.pekkoutils.SupervisorStrategies
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeShutDown, ClusterCoupled, ClusterFailedOver, ClusterPassiveLost, ClusterResetStarted, ClusterSwitchedOver}
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import js7.data.event.JournalHeaders.*
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{AnyKeyedEvent, EventId, JournalEvent, JournalHeader, KeyedEvent, SnapshotableState, Stamped}
import js7.journal.JournalActor.*
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.log.JournalLogger.Loggable
import js7.journal.watch.JournalingObserver
import js7.journal.write.{EventJournalWriter, SnapshotJournalWriter}
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Cancelable, Scheduler}
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.concurrent.{Await, Promise}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class JournalActor[S <: SnapshotableState[S]/*: diffx.Diff*/] private(
  journalLocation: JournalLocation,
  protected val conf: JournalConf,
  keyedEventBus: EventPublisher[Stamped[AnyKeyedEvent]],
  scheduler: Scheduler,
  eventIdGenerator: EventIdGenerator,
  stopped: Promise[Stopped])
  (implicit S: SnapshotableState.Companion[S])
extends Actor, Stash, JournalLogging:

  assert(journalLocation.S eq S)

  import context.{become, stop}

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val snapshotRequesters = mutable.Set.empty[ActorRef]
  private var snapshotSchedule: Cancelable = null

  private var uncommittedState: S = null.asInstanceOf[S]
  // committedState is being read asynchronously from outside this JournalActor. Always keep consistent!
  @volatile private var committedState: S = null.asInstanceOf[S]
  private val commitStateSync = new Object
  private val journaledStateBuilder = S.newBuilder()
  /** Originates from `JournalValue`, calculated from recovered journal if not freshly initialized. */
  private var journalHeader: JournalHeader = null
  private var totalRunningSince = now
  private val journalingObserver = SetOnce[Option[JournalingObserver]]
  private var eventWriter: EventJournalWriter = null
  private var snapshotWriter: SnapshotJournalWriter = null
  private var lastSnapshotTakenEventId = EventId.BeforeFirst
  private val persistBuffer = new PersistBuffer
  private var lastWrittenEventId = EventId.BeforeFirst
  private var lastAcknowledgedEventId = EventId.BeforeFirst
  private var commitDeadline: Deadline = null
  private val delayedCommit = SerialCancelable()
  private var totalEventCount = 0L
  private var fileEventCount = 0L
  private var lastSnapshotSizeEventCount = 0L
  private var lastSnapshotSize = -1L
  private var requireClusterAcknowledgement = false
  private var releaseEventIdsAfterClusterCoupledAck: Option[EventId] = None
  private val ackWarnMinimumDuration =
    conf.ackWarnDurations.headOption.getOrElse(FiniteDuration.MaxValue)
  private var waitingForAcknowledge = false
  private val waitingForAcknowledgeTimer = SerialCancelable()
  private var waitingForAcknowledgeSince = now
  private val waitingForAckSym = new BlockingSymbol
  private var isHalted = false
  private val statistics = new Statistics

  for o <- conf.simulateSync do logger.warn(s"Disk sync is simulated with a ${o.pretty} pause")
  logger.whenTraceEnabled { logger.debug("Logger isTraceEnabled=true") }

  override def postStop() =
    isHalted = true // is publicly readable via Journal#isHalted, even after actor stop
    if snapshotSchedule != null then snapshotSchedule.cancel()
    delayedCommit := Cancelable.empty // Discard commit for fast exit
    stopped.trySuccess(Stopped)
    if snapshotWriter != null then
      logger.debug(s"Deleting temporary journal files due to termination: ${snapshotWriter.file}")
      snapshotWriter.close()
      delete(snapshotWriter.file)
    if eventWriter != null then
      eventWriter.close()
    waitingForAcknowledgeTimer.cancel()
    logger.debug(s"Stopped · ${statistics.logLine}")
    super.postStop()

  def receive =
    case Input.Start(journaledState_, observer_, header, totalRunningSince_) =>
      committedState = journaledState_.asInstanceOf[S]
      uncommittedState = committedState
      if conf.slowCheckState then
        journaledStateBuilder.initializeState(None, journaledState_.eventId, totalEventCount = 0, committedState)
        assertEqualSnapshotState("Start", journaledStateBuilder.result().withEventId(uncommittedState.eventId))
      requireClusterAcknowledgement = committedState.clusterState.isInstanceOf[ClusterState.Coupled]
      journalingObserver := observer_
      journalHeader = header
      totalRunningSince = totalRunningSince_
      lastWrittenEventId = header.eventId
      totalEventCount = header.totalEventCount
      eventIdGenerator.updateLastEventId(lastWrittenEventId)
      val sender = this.sender()
      locally:
        val file = toSnapshotTemporary(journalLocation.file(after = lastWrittenEventId))
        if exists(file) then
          logger.warn(s"JournalWriter: Deleting existent file '$file'")
          delete(file)
      takeSnapshot()
      unstashAll()
      become(ready)
      sender ! Output.Ready(journalHeader)

    case _ =>
      stash()

  private def ready: Receive = receiveGet orElse:
    case Input.Store(correlId, timestamped, replyTo, options, since, commitLater, callersItem) =>
      if isHalted then
        for o <- timestamped do
          logger.debug(s"Event ignored because journal is halted: ${o.keyedEvent.toString.truncateWithEllipsis(200)}")
        // We ignore the event and do not notify the caller,
        // because it would crash and disturb the process of switching-over.
        // (so AgentDriver with AgentReady event)
        // TODO The caller should handle the error (persist method does not allow this for now)
        //reply(sender(), replyTo, Output.StoreFailure(ClusterNodeHasBeenSwitchedOverProblem, callersItem))
      else
        val stampedEvents = timestamped.view.map(t => eventIdGenerator.stamp(t.keyedEvent, t.timestampMillis)).toVector
        uncommittedState.applyStampedEvents(stampedEvents) match
          case Left(problem) =>
            reply(sender(), replyTo, Output.Stored(Left(problem),  uncommittedState, callersItem))

          case Right(updatedState) =>
            uncommittedState = updatedState
            checkUncommittedState(stampedEvents)
            eventWriter.writeEvents(stampedEvents, transaction = options.transaction)
            val lastFileLengthAndEventId = stampedEvents.lastOption.map(o => PositionAnd(eventWriter.fileLength, o.eventId))
            for o <- lastFileLengthAndEventId do
              lastWrittenEventId = o.value
            // TODO Handle serialization (but not I/O) error? writeEvents is not atomic.
            if commitLater then
              // TODO Set a timer for a later commit here?
              reply(sender(), replyTo, Output.Accepted(callersItem))
              persistBuffer.add(
                AcceptEarlyPersist(correlId, totalEventCount + 1, stampedEvents.size, since,
                  lastFileLengthAndEventId, sender()))
              // acceptEarly-events must not modify the SnapshotableState !!!
              // The EventId will be updated.
              // Ergibt falsche Reihenfolge mit dem anderen Aufruf: logCommitted(flushed = false, synced = false, stampedEvents)
            else
              persistBuffer.add(
                StandardPersist(correlId, totalEventCount + 1, stampedEvents, options.transaction, since,
                  lastFileLengthAndEventId, replyTo, sender(), callersItem))
            totalEventCount += stampedEvents.size
            fileEventCount += stampedEvents.size

            stampedEvents.lastOption match
              case Some(Stamped(eventId, _, KeyedEvent(_, _: ClusterCoupled))) =>
                // Commit now to let Coupled event take effect on following events (avoids deadlock)
                commit()
                logger.info("Cluster is coupled: Start requiring acknowledgements from passive cluster node")
                requireClusterAcknowledgement = true
                releaseEventIdsAfterClusterCoupledAck = Some(eventId)

              case Some(Stamped(_, _, KeyedEvent(_, _: ClusterSwitchedOver))) =>
                commit()
                logger.debug("SwitchedOver: no more events are accepted")
                isHalted = true  // No more events are accepted

              case Some(Stamped(_, _, KeyedEvent(_, event: ClusterFailedOver))) =>
                commitWithoutAcknowledgement(event)

              case Some(Stamped(_, _, KeyedEvent(_, event: ClusterPassiveLost))) =>
                commitWithoutAcknowledgement(event)

              case Some(Stamped(_, _, KeyedEvent(_, event: ClusterActiveNodeShutDown))) =>
                commit()
                commitWithoutAcknowledgement(event)  // No events, only switch off acks

              case Some(Stamped(_, _, KeyedEvent(_, ClusterResetStarted))) =>
                commit()
                requireClusterAcknowledgement = false

              case _ =>
                if persistBuffer.eventCount >= conf.coalesceEventLimit then
                  // Shrink persistBuffer
                  // TODO coalesce-event-limit has no effect in cluster mode, persistBuffer does not shrink
                  commit()
                else
                  forwardCommit((options.delay max conf.delay) - options.alreadyDelayed)

    case Internal.Commit =>
      if persistBuffer.isEmpty then
        logger.trace("Commit but persistBuffer.isEmpty")
      commit()

    case Input.TakeSnapshot if !isHalted =>
      logger.debug(s"TakeSnapshot ${sender()}")
      snapshotRequesters += sender()
      tryTakeSnapshotIfRequested()

    case Input.PassiveNodeAcknowledged(eventId_) =>
      var ack = eventId_
      if ack > lastWrittenEventId && isHalted then
        // The other cluster node may already have become active (uncoupled),
        // generating new EventIds whose last one we may receive here.
        // So we take the last one we know (must be the EventId of ClusterSwitchedOver)
        // TODO Can web service /api/journal suppress EventIds on passive node side after becoming active?
        lazy val msg = s"Passive cluster node acknowledged future event ${EventId.toString(ack)}" +
                  s" while lastWrittenEventId=${EventId.toString(lastWrittenEventId)} (okay when switching over)"
        if lastAcknowledgedEventId < lastWrittenEventId then logger.warn(msg) else logger.debug(msg)
        ack = lastWrittenEventId
      sender() ! Completed
      // The passive node does not know Persist bundles (maybe transactions) and acknowledges events as they arrive.
      // We take only complete Persist bundles as acknowledged.
      onCommitAcknowledged(
        n = persistBuffer.iterator.takeWhile(_.lastStamped.forall(_.eventId <= ack)).length,
        ack = Some(ack))
      if releaseEventIdsAfterClusterCoupledAck.isDefined then
        releaseObsoleteEvents()

    case Input.PassiveLost(passiveLost) =>
      // Side channel for Cluster to circumvent the ClusterEvent synchronization lock
      // in case of a concurrent open persist operation.
      // Must be followed by a ClusterPassiveLost event.
      commitWithoutAcknowledgement(passiveLost)
      sender() ! Completed

    case Input.Terminate =>
      logger.debug("Terminate")
      if !isHalted then
        commit(terminating = true)
        closeEventWriter()
      stop(self)

    case Internal.StillWaitingForAcknowledge =>
      if requireClusterAcknowledgement && lastAcknowledgedEventId < lastWrittenEventId then
        val n = persistBuffer.view.map(_.eventCount).sum
        if n > 0 then
          waitingForAckSym.onInfo()
          val lastEvent = persistBuffer.view
            .collect { case o: StandardPersist => o }
            .takeWhile(_.since.elapsed >= ackWarnMinimumDuration)
            .flatMap(_.stampedSeq).lastOption.fold("(unknown)")(_
            .toString.truncateWithEllipsis(200))
          logger.warn(s"$waitingForAckSym Waiting for ${waitingForAcknowledgeSince.elapsed.pretty}" +
            " for acknowledgement from passive cluster node" +
            s" for $n events (in ${persistBuffer.size} persists), last is $lastEvent" +
            s", lastAcknowledgedEventId=${EventId.toString(lastAcknowledgedEventId)}")
        else
          logger.trace(s"StillWaitingForAcknowledge n=0, persistBuffer.size=${persistBuffer.size}")
      else
        waitingForAcknowledgeTimer := Cancelable.empty

  private def forwardCommit(delay: FiniteDuration): Unit =
    val deadline = now + delay
    if commitDeadline == null || deadline < commitDeadline then
      commitDeadline = deadline
      if delay.isZeroOrBelow then
        self.forward(Internal.Commit)
      else
        val sender = context.sender()
        delayedCommit := scheduler.scheduleOnce(deadline.timeLeft):
          self.tell(Internal.Commit, sender)  // Don't  use forward in async operation

  /** Flushes and syncs the already written events to disk, then notifying callers and EventBus. */
  private def commit(terminating: Boolean = false): Unit =
    commitDeadline = null
    delayedCommit := Cancelable.empty
    if persistBuffer.nonEmpty then
      try eventWriter.flush(sync = conf.syncOnCommit)
      catch { case NonFatal(t) if !terminating =>
        val tt = t.appendCurrentStackTrace
        //persistBuffer.view foreach {
        //  case w: StandardPersist => reply(sender(), w.replyTo, Output.StoreFailure(Problem.pure(tt), CALLERS_ITEM))  // TODO Failing flush is fatal
        //  case _: AcceptEarlyPersist =>  // TODO Error handling?
        //}
        throw tt;
      }

      persistBuffer.view.reverse
        .collectFirst { case o: StandardPersist if !o.isEmpty => o }
        .foreach { _.isLastOfFlushedOrSynced = true }

      if !terminating then
        onReadyForAcknowledgement()

  private def onReadyForAcknowledgement(): Unit =
    if !requireClusterAcknowledgement then
      onCommitAcknowledged(persistBuffer.size)
    else
      val nonEventWrittenCount = persistBuffer.iterator.takeWhile(_.isEmpty).size
      if nonEventWrittenCount > 0 then
        // `Persist` without events (Nil) are not being acknowledged, so we finish them now
        onCommitAcknowledged(nonEventWrittenCount)
      startWaitingForAcknowledgeTimer()

  private def startWaitingForAcknowledgeTimer(): Unit =
    if requireClusterAcknowledgement && lastAcknowledgedEventId < lastWrittenEventId then
      if !waitingForAcknowledge then
        waitingForAcknowledge = true
        waitingForAcknowledgeSince = now
        waitingForAcknowledgeTimer := scheduler.scheduleAtFixedRates(conf.ackWarnDurations):
          self ! Internal.StillWaitingForAcknowledge

  private def commitWithoutAcknowledgement(event: ClusterEvent): Unit =
    if requireClusterAcknowledgement then
      logger.debug(s"No more acknowledgments required due to $event event")
      waitingForAckSym.clear()
      requireClusterAcknowledgement = false
      waitingForAcknowledge = false
      waitingForAcknowledgeTimer := Cancelable.empty
    commit()

  private def onCommitAcknowledged(n: Int, ack: Option[EventId] = None): Unit =
    for ackEventId <- ack if n > 0 && waitingForAckSym.called do
      logger.info(s"🟢 $n events until $ackEventId have finally been acknowledged after ${
        waitingForAcknowledgeSince.elapsed.pretty}")
      waitingForAckSym.clear()
    finishCommitted(n, ack = ack.isDefined)
    if lastAcknowledgedEventId == lastWrittenEventId then
      onAllCommitsFinished()

  private def finishCommitted(n: Int, ack: Boolean): Unit =
    val ackWritten = persistBuffer.view.take(n)
    journalLogger.logCommitted(ackWritten, ack = ack)

    commitStateSync.synchronized:
      for lastFileLengthAndEventId <- ackWritten.flatMap(_.lastFileLengthAndEventId).lastOption do
        lastAcknowledgedEventId = lastFileLengthAndEventId.value
        eventWriter.onCommitted(lastFileLengthAndEventId, n = ackWritten.map(_.eventCount).sum)

      for persist <- ackWritten.collect { case o: StandardPersist => o } do
        committedState = committedState.applyStampedEvents(persist.stampedSeq)
          .orThrow /*may crash JournalActor !!!*/
        continueCallers(persist)

      // Update EventId for trailing acceptEarly events
      ackWritten.lastOption
        .collect { case o: AcceptEarlyPersist => o }
        .flatMap(_.lastFileLengthAndEventId)
        .foreach { case PositionAnd(_, eventId) =>
          committedState = committedState.withEventId(eventId)
        }

    for p <- persistBuffer.iterator.take(n) do statistics.onPersisted(p.eventCount, p.since)
    persistBuffer.removeFirst(n)
    assertThat((lastAcknowledgedEventId == lastWrittenEventId) == persistBuffer.isEmpty)

  private def onAllCommitsFinished(): Unit =
    assertThat(lastAcknowledgedEventId == lastWrittenEventId)
    assertThat(persistBuffer.isEmpty)
    if conf.slowCheckState && committedState != uncommittedState then
      val msg = "SnapshotableState update mismatch: committedState != uncommittedState"
      logger.error(msg)
      //diffx implicit val showConfig = diffx.ShowConfig.default.copy(
      //diffx   arrow = _.replace("->", "->>"),
      //diffx   transformer = diffx.DiffResultTransformer.skipIdentical)
      //diffx logger.error(diffx.compare(committedState, uncommittedState).show())
      sys.error(msg)
    uncommittedState = committedState    // Reduce duplicate allocated objects
    if conf.slowCheckState then
      assertEqualSnapshotState("onAllCommitsFinished",
        journaledStateBuilder.result().withEventId(committedState.eventId))
    waitingForAcknowledge = false
    waitingForAcknowledgeTimer := Cancelable.empty
    maybeDoASnapshot()

  private def continueCallers(persist: StandardPersist): Unit =
    if persist.replyTo != Actor.noSender then
      // Continue caller
      reply(persist.sender, persist.replyTo,
        Output.Stored(Right(persist.stampedSeq), committedState, persist.callersItem))

    for stamped <- persist.stampedSeq do
      keyedEventBus.publish(stamped)
      handleJournalEvents(stamped)

  private def handleJournalEvents(stamped: Stamped[AnyKeyedEvent]): Unit =
    stamped.value match
      case KeyedEvent(_: NoKey, SnapshotTaken) =>
        releaseObsoleteEvents()
        responseAfterSnapshotTaken()

      case KeyedEvent(_, _: JournalEventsReleased) =>
        releaseObsoleteEvents()

      case _ =>

  private def maybeDoASnapshot(): Unit =
    if snapshotRequesters.isEmpty &&
      eventWriter.bytesWritten >= conf.snapshotSizeLimit &&
      fileEventCount >= 2 * estimatedSnapshotSize then
      logger.debug(s"Take snapshot because written size ${toKBGB(eventWriter.bytesWritten)} is above the limit ${toKBGB(conf.snapshotSizeLimit)}")
      snapshotRequesters += self
    tryTakeSnapshotIfRequested()  // TakeSnapshot has been delayed until last event has been acknowledged
    if snapshotSchedule == null then
      snapshotSchedule = scheduler.scheduleOnce(conf.snapshotPeriod):
        if !isHalted then
          self ! Input.TakeSnapshot

  private def estimatedSnapshotSize =
    if fileEventCount > lastSnapshotSizeEventCount + conf.snapshotSizeEstimateEventThreshold then
      lastSnapshotSizeEventCount = fileEventCount
      lastSnapshotSize = committedState.estimatedSnapshotSize
    lastSnapshotSize

  private def reply(sender: ActorRef, replyTo: ActorRef, msg: Any): Unit =
    replyTo.!(msg)(sender)

  private def receiveGet: Receive =
    case Input.GetJournalActorState =>
      sender() ! Output.JournalActorState(
        isFlushed = eventWriter != null && eventWriter.isFlushed,
        isSynced = eventWriter != null && eventWriter.isSynced,
        isRequiringClusterAcknowledgement = requireClusterAcknowledgement)

    case Input.GetJournaledState =>
      // Allow the caller outside of this JournalActor to read committedState
      // asynchronously at any time.
      // Returned function accesses committedState directly and asynchronously !!!
      sender() ! (() => commitStateSync.synchronized(committedState))

    case Input.GetIsHaltedFunction =>
      // Allow the caller outside of this JournalActor to read isHalted
      // asynchronously at any time.
      sender() ! (() => isHalted)

  def tryTakeSnapshotIfRequested(): Unit =
    if snapshotRequesters.nonEmpty then
      if lastWrittenEventId == lastSnapshotTakenEventId then
        responseAfterSnapshotTaken()
      else if lastAcknowledgedEventId < lastWrittenEventId then
        logger.debug(s"Delaying snapshot until last event has been committed and acknowledged (lastAcknowledgedEventId=$lastAcknowledgedEventId lastWrittenEventId=$lastWrittenEventId)")
      else
        takeSnapshot()

  private def responseAfterSnapshotTaken(): Unit =
    for sender <- snapshotRequesters if sender != self do sender ! Output.SnapshotTaken
    snapshotRequesters.clear()

  private def takeSnapshot() =
    val since = now
    val snapshotTaken = eventIdGenerator.stamp(KeyedEvent(JournalEvent.SnapshotTaken))

    if snapshotSchedule != null then
      snapshotSchedule.cancel()
      snapshotSchedule = null
    if eventWriter != null then
      if persistBuffer.nonEmpty then  // Unfortunately we must avoid a recursion, because commit() may try a snapshot again
        commit()
      closeEventWriter()

    assertThat(journalHeader != null)
    journalHeader = journalHeader.nextGeneration[S](
      eventId = lastWrittenEventId,
      totalEventCount = totalEventCount,
      totalRunningTime = totalRunningSince.elapsed roundUpToNext 1.ms)
    val file = journalLocation.file(after = lastWrittenEventId)

    logger.info(
      s"Starting new journal file #${journalHeader.generation} ${file.getFileName} with a snapshot "
      + (if conf.syncOnCommit then "(using sync)" else "(no sync)"))
    logger.debug(journalHeader.toString)

    snapshotWriter = new SnapshotJournalWriter(journalLocation.S, toSnapshotTemporary(file), after = lastWrittenEventId,
      simulateSync = conf.simulateSync)(scheduler)
    snapshotWriter.writeHeader(journalHeader)
    snapshotWriter.beginSnapshotSection()
    //journalLogger.logHeader(journalHeader)

    locally:
      lazy val checkingBuilder = S.newBuilder()

      val future = committedState.toSnapshotObservable
        .filter:
          case SnapshotEventId(_) => false  // JournalHeader contains already the EventId
          case _ => true
        .mapParallelBatch() { snapshotObject =>
          logger.trace(s"Snapshot ${snapshotObject.toString.truncateWithEllipsis(200)}")
          snapshotObject -> snapshotObject.asJson(S.snapshotObjectJsonCodec).toByteArray
        }
        .foreach { case (snapshotObject, byteArray) =>
          if conf.slowCheckState then checkingBuilder.addSnapshotObject(snapshotObject)
          snapshotWriter.writeSnapshot(byteArray)
        }(scheduler)
      // TODO Do not block the thread
      Await.result(future, 999.s)

      if conf.slowCheckState then
        // Simulate recovery
        checkingBuilder.onAllSnapshotsAdded()
        assertEqualSnapshotState("Written snapshot",
          checkingBuilder.result().withEventId(journalHeader.eventId))

    snapshotWriter.endSnapshotSection()
    // Write a SnapshotTaken event to increment EventId and force a new filename
    snapshotWriter.beginEventSection(sync = false)
    val (fileLengthBeforeEvents, fileEventId) = (snapshotWriter.fileLength, lastWrittenEventId)

    snapshotWriter.writeEvent(snapshotTaken)
    snapshotWriter.flush(sync = conf.syncOnCommit)
    locally:
      val standardPersist = StandardPersist(
        CorrelId.empty, totalEventCount + 1, snapshotTaken :: Nil, false, since,
        Some(PositionAnd(snapshotWriter.fileLength, snapshotTaken.eventId)),
        Actor.noSender, null, null)
      standardPersist.isLastOfFlushedOrSynced = true
      persistBuffer.add(standardPersist)
    lastWrittenEventId = snapshotTaken.eventId
    uncommittedState = uncommittedState.applyStampedEvents(snapshotTaken :: Nil).orThrow
    if !requireClusterAcknowledgement then
      lastAcknowledgedEventId = lastWrittenEventId
    totalEventCount += 1
    fileEventCount = 1
    lastSnapshotTakenEventId = snapshotTaken.eventId
    snapshotWriter.closeAndLog()

    move(snapshotWriter.file, journalLocation.file(after = fileEventId), ATOMIC_MOVE)
    snapshotWriter = null

    eventWriter = newEventJsonWriter(after = fileEventId)
    eventWriter.onJournalingStarted(fileLengthBeforeEvents = fileLengthBeforeEvents)

    onReadyForAcknowledgement()

    val how = if (conf.syncOnCommit) "(with sync)" else "(without sync)"
    logger.debug(s"Snapshot written $how to journal file ${eventWriter.file.getFileName}")

  private def newEventJsonWriter(after: EventId, withoutSnapshots: Boolean = false) =
    assertThat(journalHeader != null)
    val file = journalLocation.file(after = after)
    val w = new EventJournalWriter(journalLocation.S, file,
      after = after, journalHeader.journalId,
      journalingObserver.orThrow, simulateSync = conf.simulateSync,
      withoutSnapshots = withoutSnapshots, initialEventCount = 1/*SnapshotTaken*/)(scheduler)
    journalLocation.updateSymbolicLink(file)
    w

  private def closeEventWriter(): Unit =
    if eventWriter != null then
      if isHalted then
        eventWriter.flush(sync = conf.syncOnCommit)
        eventWriter.close()
      else
        eventWriter.closeProperly(sync = conf.syncOnCommit)
      eventWriter = null

  private def releaseObsoleteEvents(): Unit =
    if conf.deleteObsoleteFiles then
      if committedState.clusterState == ClusterState.Empty ||
          requireClusterAcknowledgement
            // ClusterPassiveLost after SnapshotTaken in the same commit chunk has reset
            // requireClusterAcknowledgement. We must not delete the file when cluster is being decoupled.
          && (committedState.clusterState.isInstanceOf[ClusterState.Coupled] ||
              committedState.clusterState.isInstanceOf[ClusterState.ActiveShutDown])
          && releaseEventIdsAfterClusterCoupledAck.forall(_ <= lastAcknowledgedEventId) then
        val eventId =
          if committedState.clusterState == ClusterState.Empty then
            lastWrittenEventId
          else
            // Do not release the just acknowledged last Event of a journal file
            // (i.e. the last acknowledged event is the last event of a journal file)
            // because after restart, the passive node continues reading the journal file
            // (only to get end-of-file). Subtract 1 to avoid this.
            (lastAcknowledgedEventId - 1) max EventId.BeforeFirst
        releaseObsoleteEventsUntil(committedState.journalState.toReleaseEventId(eventId, conf.releaseEventsUserIds))
        releaseEventIdsAfterClusterCoupledAck = None

  private def releaseObsoleteEventsUntil(untilEventId: EventId): Unit =
    logger.debug(s"releaseObsoleteEvents($untilEventId) ${committedState.journalState}, clusterState=${committedState.clusterState}")
    journalingObserver.orThrow match
      case Some(o) =>
        o.releaseEvents(untilEventId)(scheduler)
      case None =>
        // Without a JournalingObserver, we can delete all previous journal files (for Agent)
        val until = untilEventId min journalHeader.eventId
        for j <- journalLocation.listJournalFiles if j.fileEventId < until do
          val file = j.file
          assertThat(file != eventWriter.file)
          try delete(file)
          catch { case NonFatal(t) =>
            logger.warn(s"Cannot delete obsolete journal file '$file': ${t.toStringWithCauses}")
          }

  private def checkUncommittedState(stampedEvents: Seq[Stamped[AnyKeyedEvent]]): Unit =
    if conf.slowCheckState then
      stampedEvents.foreach(journaledStateBuilder.addEvent)
      assertEqualSnapshotState("SnapshotableStateBuilder.result()",
        journaledStateBuilder.result().withEventId(uncommittedState.eventId),
        stampedEvents)

      // Check recoverability
      implicit val s = scheduler
      assertEqualSnapshotState("toRecovered", uncommittedState.toRecovered.runSyncUnsafe(99.s),
        stampedEvents)

      val builder = S.newBuilder()
      builder.initializeState(None, uncommittedState.eventId, totalEventCount, uncommittedState)
      assertEqualSnapshotState("Builder.initializeState",
        builder.result().withEventId(uncommittedState.eventId),
        stampedEvents)

  private def assertEqualSnapshotState(what: String, couldBeRecoveredState: S,
    stampedSeq: Seq[Stamped[AnyKeyedEvent]] = Nil)
  : Unit =
    if couldBeRecoveredState != uncommittedState then
      var msg = s"$what does not match actual '$S'"
      logger.error(msg)
      for stamped <- stampedSeq do logger.error(stamped.toString.truncateWithEllipsis(200))
      // msg may get very big
      //diffx msg ++= ":\n" ++ diffx.compare(couldBeRecoveredState, uncommittedState).show()
      logger.info(msg)  // Without colors because msg is already colored
      throw new AssertionError(msg)


object JournalActor:
  private val logger = Logger[this.type]
  private val TmpSuffix = ".tmp"  // Duplicate in PassiveClusterNode

  //private val ClusterNodeHasBeenSwitchedOverProblem = Problem.pure("After switchover, this cluster node is no longer active")

  def props[S <: SnapshotableState[S]: SnapshotableState.Companion/*: diffx.Diff*/](
    journalLocation: JournalLocation,
    conf: JournalConf,
    keyedEventBus: EventPublisher[Stamped[AnyKeyedEvent]],
    scheduler: Scheduler,
    eventIdGenerator: EventIdGenerator,
    stopped: Promise[Stopped] = Promise())
  =
    Props:
      new JournalActor[S](journalLocation, conf, keyedEventBus, scheduler, eventIdGenerator, stopped)

  private def toSnapshotTemporary(file: Path) = file.resolveSibling(s"${file.getFileName}$TmpSuffix")

  private[journal] trait CallersItem

  object Input:
    private[journal] final case class Start[S <: SnapshotableState[S]](
      journaledState: SnapshotableState[S],
      journalingObserver: Option[JournalingObserver],
      recoveredJournalHeader: JournalHeader,
      totalRunningSince: Deadline)

    private[journal] final case class Store(
      correlId: CorrelId,
      timestamped: Seq[Timestamped],
      journalingActor: ActorRef,
      options: CommitOptions,
      since: Deadline,
      commitLater: Boolean = false,
      callersItem: CallersItem)

    case object TakeSnapshot
    final case class PassiveNodeAcknowledged(eventId: EventId)
    final case class PassiveLost(passiveLost: ClusterPassiveLost)
    case object Terminate
    case object GetJournalActorState
    case object GetJournaledState
    case object GetIsHaltedFunction

  private[journal] trait Timestamped:
    def keyedEvent: AnyKeyedEvent
    def timestampMillis: Option[Long]

  sealed trait Output
  object Output:
    final case class Ready(journalHeader: JournalHeader)

    private[journal] final case class Stored[S <: SnapshotableState[S]](
      stamped: Checked[Seq[Stamped[AnyKeyedEvent]]],
      journaledState: S,
      callersItem: CallersItem)
    extends Output

    private[journal] final case class Accepted(callersItem: CallersItem) extends Output
    //final case class SerializationFailure(throwable: Throwable) extends Output
    case object SnapshotTaken
    final case class JournalActorState(isFlushed: Boolean, isSynced: Boolean, isRequiringClusterAcknowledgement: Boolean)

  type Stopped = Stopped.type
  case object Stopped

  private object Internal:
    case object Commit
    case object StillWaitingForAcknowledge extends DeadLetterSuppression

  private class PersistBuffer:
    private val buffer = mutable.ArrayBuffer.empty[Persist]
    private var _eventCount = 0

    def add(persist: Persist): Unit =
      buffer += persist
      persist match
        case persist: StandardPersist => _eventCount += persist.stampedSeq.size
        case _ =>

    def removeFirst(n: Int): Unit =
      buffer.view.take(n) foreach:
        case persist: StandardPersist => _eventCount -= persist.stampedSeq.size
        case _ =>
      buffer.remove(0, n)
      assertThat(buffer.nonEmpty || eventCount == 0)

    def isEmpty = buffer.isEmpty

    def nonEmpty = buffer.nonEmpty

    def size = buffer.size

    def view = buffer.view

    def iterator = buffer.iterator

    def eventCount = _eventCount

  private[journal] sealed trait Persist extends Loggable:
    def eventNumber: Long
    def eventCount: Int
    def stampedSeq: Seq[Stamped[AnyKeyedEvent]]
    def isEmpty = stampedSeq.isEmpty
    def isTransaction: Boolean
    def lastFileLengthAndEventId: Option[PositionAnd[EventId]]
    def lastStamped: Option[Stamped[AnyKeyedEvent]]
    def since: Deadline
    def isLastOfFlushedOrSynced: Boolean

  /** A bundle of written but not yet committed (flushed and acknowledged) Persists. */
  private final case class StandardPersist(
    correlId: CorrelId,
    eventNumber: Long,
    stampedSeq: Seq[Stamped[AnyKeyedEvent]],
    isTransaction: Boolean,
    since: Deadline,
    lastFileLengthAndEventId: Option[PositionAnd[EventId]],
    replyTo: ActorRef,
    sender: ActorRef,
    callersItem: CallersItem)
  extends Persist:
    def eventCount = stampedSeq.size

    def lastStamped: Option[Stamped[AnyKeyedEvent]] =
      stampedSeq.reverseIterator.buffered.headOption

    /** For logging: last stamped (and all before) has been flushed or synced */
    var isLastOfFlushedOrSynced = false

  // Without event to keep heap usage low (especially for many big stdout event)
  private final case class AcceptEarlyPersist(
    correlId: CorrelId,
    eventNumber: Long,
    eventCount: Int,
    since: Deadline,
    lastFileLengthAndEventId: Option[PositionAnd[EventId]],
    sender: ActorRef)
  extends Persist:
    /** The events are not shown here */
    def stampedSeq = Nil
    def isTransaction = false
    def lastStamped = None
    def isLastOfFlushedOrSynced = false

  private class Statistics:
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

    def logLine: String =
      if persistCount == 0 then
        ""
      else
        s"$persistCount persists" +
          f" · $eventCount events (${1.0 * eventCount / persistCount}%.1f/persist)" +
          s" ${persistDurationMin.ns.pretty}" +
          s" ... ∅ ${persistDurationAvg.pretty}" +
          s" ... ${persistDurationMax.ns.pretty}"

    private def persistDurationAvg = (persistDurationSum / persistCount).ns

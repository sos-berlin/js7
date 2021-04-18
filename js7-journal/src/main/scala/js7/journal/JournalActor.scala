package js7.journal

import akka.actor.{Actor, ActorRef, DeadLetterSuppression, Props, Stash}
import com.softwaremill.diffx
import io.circe.syntax.EncoderOps
import java.nio.file.Files.{delete, exists, move}
import java.nio.file.Path
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import js7.base.circeutils.CirceUtils._
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.common.akkautils.SupervisorStrategies
import js7.common.jsonseq.PositionAnd
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import js7.data.event.JournalHeaders._
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{AnyKeyedEvent, EventId, JournalEvent, JournalHeader, JournalHeaders, JournalId, JournaledState, KeyedEvent, Stamped}
import js7.journal.JournalActor._
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalMeta
import js7.journal.files.JournalFiles.{JournalMetaOps, listJournalFiles}
import js7.journal.watch.JournalingObserver
import js7.journal.write.{EventJournalWriter, SnapshotJournalWriter}
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Cancelable, Scheduler}
import scala.collection.{View, mutable}
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, Duration, FiniteDuration}
import scala.concurrent.{Await, Promise, blocking}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class JournalActor[S <: JournaledState[S]: diffx.Diff] private(
  journalMeta: JournalMeta,
  conf: JournalConf,
  keyedEventBus: StampedKeyedEventBus,
  scheduler: Scheduler,
  eventIdGenerator: EventIdGenerator,
  stopped: Promise[Stopped])
  (implicit S: JournaledState.Companion[S])
extends Actor with Stash
{
  import context.{become, stop}

  override val supervisorStrategy = SupervisorStrategies.escalate
  private val snapshotRequesters = mutable.Set.empty[ActorRef]
  private var snapshotSchedule: Cancelable = null

  private var uncommittedState: S = null.asInstanceOf[S]
  // committedState is being read asynchronously from outside this JournalActor. Always keep consistent!
  @volatile private var committedState: S = null.asInstanceOf[S]
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
  private var delayedCommit = SerialCancelable()
  private var totalEventCount = 0L
  private var fileEventCount = 0L
  private var requireClusterAcknowledgement = false
  private var releaseEventIdsAfterClusterCoupledAck: Option[EventId] = None
  private val waitingForAcknowledgeTimer = SerialCancelable()
  private var waitingForAcknowledgeSince = now
  private var switchedOver = false

  for (o <- conf.simulateSync) logger.warn(s"Disk sync is simulated with a ${o.pretty} pause")
  logger.whenTraceEnabled { logger.debug("Logger isTraceEnabled=true") }

  override def postStop() = {
    if (snapshotSchedule != null) snapshotSchedule.cancel()
    delayedCommit := Cancelable.empty // Discard commit for fast exit
    stopped.trySuccess(Stopped)
    if (snapshotWriter != null) {
      logger.debug(s"Deleting temporary journal files due to termination: ${snapshotWriter.file}")
      snapshotWriter.close()
      delete(snapshotWriter.file)
    }
    if (eventWriter != null) {
      eventWriter.close()
    }
    waitingForAcknowledgeTimer.cancel()
    logger.debug("Stopped")
    super.postStop()
  }

  def receive = {
    case Input.Start(journaledState_, observer_, header, totalRunningSince_) =>
      committedState = journaledState_.asInstanceOf[S]
      uncommittedState = committedState
      if (conf.slowCheckState) {
        journaledStateBuilder.initializeState(None, journaledState_.eventId, totalEventCount = 0, committedState)
      }
      requireClusterAcknowledgement = committedState.clusterState.isInstanceOf[ClusterState.Coupled]
      journalingObserver := observer_
      journalHeader = header
      totalRunningSince = totalRunningSince_
      lastWrittenEventId = header.eventId
      totalEventCount = header.totalEventCount
      eventIdGenerator.updateLastEventId(lastWrittenEventId)
      val sender = this.sender()
      locally {
        val file = toSnapshotTemporary(journalMeta.file(after = lastWrittenEventId))
        if (exists(file)) {
          logger.warn(s"JournalWriter: Deleting existent file '$file'")
          delete(file)
        }
      }
      takeSnapshot()
      unstashAll()
      become(ready)
      logReady()
      sender ! Output.Ready(journalHeader)

    case Input.StartWithoutRecovery(journalId, observer_) =>  // Testing only
      committedState = S.empty
      uncommittedState = committedState
      if (conf.slowCheckState) {
        journaledStateBuilder.initializeState(None, committedState.eventId, totalEventCount = 0, committedState)
      }
      journalingObserver := observer_
      journalHeader = JournalHeaders.initial(journalId).copy(generation = 1)
      eventWriter = newEventJsonWriter(after = EventId.BeforeFirst, withoutSnapshots = true)
      eventWriter.writeHeader(journalHeader)
      eventWriter.beginEventSection(sync = conf.syncOnCommit)
      eventWriter.onJournalingStarted()
      unstashAll()
      become(ready)
      logReady()
      sender() ! Output.Ready(journalHeader)

    case _ =>
      stash()
  }

  private def logReady() =
    logger.info(s"Ready, writing ${if (conf.syncOnCommit) "(with sync)" else "(without sync)"} journal file '${eventWriter.file.getFileName}'")

  private def ready: Receive = receiveGet orElse {
    case Input.Store(timestamped, replyTo, acceptEarly, transaction, delay, alreadyDelayed, since, callersItem) =>
      if (switchedOver) {
        for (o <- timestamped) {
          logger.debug(s"Event ignored while active cluster node is switching over: ${o.keyedEvent.toString.truncateWithEllipsis(200)}")
        }
        // We ignore the event and do not notify the caller,
        // because it would crash and disturb the process of switching-over.
        // (so AgentDriver with AgentReady event)
        // TODO The caller should handle the error (persist method does not allow this for now)
        //reply(sender(), replyTo, Output.StoreFailure(ClusterNodeHasBeenSwitchedOverProblem, callersItem))
      } else {
        val stampedEvents = timestamped.view.map(t => eventIdGenerator.stamp(t.keyedEvent, t.timestampMillis)).toVector
        uncommittedState.applyStampedEvents(stampedEvents) match {
          case Left(problem) =>
            logger.error(problem.toString)
            for (stamped <- stampedEvents) logger.error(stamped.toString)
            reply(sender(), replyTo, Output.StoreFailure(problem, callersItem))

          case Right(updatedState) =>
            uncommittedState = updatedState
            if (conf.slowCheckState) {
              stampedEvents.foreach(journaledStateBuilder.addEvent)
              assertEqualSnapshotState("JournaledStateBuilder.result()",
                journaledStateBuilder.result().withEventId(uncommittedState.eventId),
                stampedEvents)
            }
            eventWriter.writeEvents(stampedEvents, transaction = transaction)
            val lastFileLengthAndEventId = stampedEvents.lastOption.map(o => PositionAnd(eventWriter.fileLength, o.eventId))
            for (o <- lastFileLengthAndEventId) {
              lastWrittenEventId = o.value
            }
            // TODO Handle serialization (but not I/O) error? writeEvents is not atomic.
            if (acceptEarly && !requireClusterAcknowledgement/*? irrelevant because acceptEarly is not used in a Cluster for now*/) {
              reply(sender(), replyTo, Output.Accepted(callersItem))
              persistBuffer.add(
                AcceptEarlyPersist(stampedEvents.size, since, lastFileLengthAndEventId, sender()))
              // acceptEarly-events must not modify the JournaledState !!!
              // The EventId will be updated.
              // Ergibt falsche Reihenfolge mit dem anderen Aufruf: logCommitted(flushed = false, synced = false, stampedEvents)
            } else {
              persistBuffer.add(
                StandardPersist(totalEventCount + 1, stampedEvents, since, lastFileLengthAndEventId, replyTo, sender(), callersItem))
            }
            totalEventCount += stampedEvents.size
            fileEventCount += stampedEvents.size

            stampedEvents.lastOption match {
              case Some(Stamped(eventId, _, KeyedEvent(_, _: ClusterCoupled))) =>
                // Commit now to let Coupled event take effect on following events (avoids deadlock)
                commit()
                logger.info(s"Cluster is coupled: Start requiring acknowledgements from passive cluster node")
                requireClusterAcknowledgement = true
                releaseEventIdsAfterClusterCoupledAck = Some(eventId)

              case Some(Stamped(_, _, KeyedEvent(_, _: ClusterSwitchedOver))) =>
                commit()
                logger.debug("SwitchedOver: no more events are accepted")
                switchedOver = true  // No more events are accepted

              case Some(Stamped(_, _, KeyedEvent(_, event: ClusterFailedOver))) =>
                commitWithoutAcknowledgement(event)

              case Some(Stamped(_, _, KeyedEvent(_, event: ClusterPassiveLost))) =>
                commitWithoutAcknowledgement(event)

              case _ =>
                if (persistBuffer.eventCount >= conf.coalesceEventLimit) {
                  // Shrink persistBuffer
                  // TODO coalesce-event-limit has no effect in cluster mode, persistBuffer does not shrink
                  commit()
                } else {
                  forwardCommit((delay max conf.delay) - alreadyDelayed)
                }
            }
          }
      }

    case Internal.Commit =>
      if (persistBuffer.isEmpty) {
        logger.trace("Commit but persistBuffer.isEmpty")
      }
      commit()

    case Input.TakeSnapshot if !switchedOver =>
      logger.debug(s"TakeSnapshot ${sender()}")
      snapshotRequesters += sender()
      tryTakeSnapshotIfRequested()

    case Input.PassiveNodeAcknowledged(eventId_) =>
      var ack = eventId_
      if (ack > lastWrittenEventId && switchedOver) {
        // The other cluster node may already have become active (uncoupled),
        // generating new EventIds whose last one we may receive here.
        // So we take the last one we know (must be the EventId of ClusterSwitchedOver)
        // TODO Can web service /api/journal suppress EventIds on passive node side after becoming active?
        lazy val msg = s"Passive cluster node acknowledged future event ${EventId.toString(ack)}" +
                  s" while lastWrittenEventId=${EventId.toString(lastWrittenEventId)} (okay when switching over)"
        if (lastAcknowledgedEventId < lastWrittenEventId) logger.warn(msg) else logger.debug(msg)
        ack = lastWrittenEventId
      }
      sender() ! Completed
      // The passive node does not know Persist bundles (maybe transactions) and acknowledges events as they arrive.
      // We take only complete Persist bundles as acknowledged.
      onCommitAcknowledged(n = persistBuffer.iterator.takeWhile(_.lastStamped.forall(_.eventId <= ack)).length)
      if (releaseEventIdsAfterClusterCoupledAck.isDefined) {
        releaseObsoleteEvents()
      }

    case Input.PassiveLost(passiveLost) =>
      // Side channel for Cluster to circumvent the ClusterEvent synchronization lock
      // in case of a concurrent open persist operation.
      // Must be followed by a ClusterPassiveLost event.
      commitWithoutAcknowledgement(passiveLost)
      sender() ! Completed

    case Input.Terminate =>
      logger.debug("Terminate")
      if (!switchedOver) {
        commit(terminating = true)
        closeEventWriter()
      }
      stop(self)

    case Internal.StillWaitingForAcknowledge =>
      if (requireClusterAcknowledgement && lastAcknowledgedEventId < lastWrittenEventId) {
        val notAckSeq = persistBuffer.view.collect { case o: LoggablePersist => o }
          .takeWhile(_.since.elapsed >= conf.ackWarnDurations.headOption.getOrElse(FiniteDuration.MaxValue))
        val n = persistBuffer.view.map(_.eventCount).sum
        if (n > 0) {
          logger.warn(s"Waiting for ${waitingForAcknowledgeSince.elapsed.pretty}" +
            " for acknowledgement from passive cluster node" +
            s" for $n events (in ${persistBuffer.size} persists), last is " +
            notAckSeq.flatMap(_.stampedSeq).lastOption.fold("(unknown)")(_.toString.truncateWithEllipsis(200)) +
            s", lastAcknowledgedEventId=${EventId.toString(lastAcknowledgedEventId)}")
        } else logger.debug(s"StillWaitingForAcknowledge n=0, persistBuffer.size=${persistBuffer.size}")
      } else {
        waitingForAcknowledgeTimer := Cancelable.empty
      }
  }

  private def forwardCommit(delay: FiniteDuration = Duration.Zero): Unit = {
    val deadline = now + delay
    if (commitDeadline == null || deadline < commitDeadline) {
      commitDeadline = deadline
      if (!delay.isPositive) {
        self.forward(Internal.Commit)
      } else {
        val sender = context.sender()
        delayedCommit := scheduler.scheduleOnce(deadline.timeLeft) {
          self.tell(Internal.Commit, sender)  // Don't  use forward in async operation
        }
      }
    }
  }

  /** Flushes and syncs the already written events to disk, then notifying callers and EventBus. */
  private def commit(terminating: Boolean = false): Unit = {
    commitDeadline = null
    delayedCommit := Cancelable.empty
    if (persistBuffer.nonEmpty) {
      try eventWriter.flush(sync = conf.syncOnCommit)
      catch { case NonFatal(t) if !terminating =>
        val tt = t.appendCurrentStackTrace
        //persistBuffer.view foreach {
        //  case w: StandardPersist => reply(sender(), w.replyTo, Output.StoreFailure(Problem.pure(tt), CALLERS_ITEM))  // TODO Failing flush is fatal
        //  case _: AcceptEarlyPersist =>  // TODO Error handling?
        //}
        throw tt;
      }
      for (w <- persistBuffer.view.reverse collectFirst { case o: LoggablePersist if !o.isEmpty => o }) {
        w.isLastOfFlushedOrSynced = true  // For logging: This last Persist (including all before) has been flushed or synced
      }
      if (!terminating) {
        onReadyForAcknowledgement()
      }
    }
  }

  private def onReadyForAcknowledgement(): Unit = {
    if (!requireClusterAcknowledgement) {
      onCommitAcknowledged(persistBuffer.size)
    } else {
      val nonEventWrittenCount = persistBuffer.iterator.takeWhile(_.isEmpty).size
      if (nonEventWrittenCount > 0) {
        // `Persist` without events (Nil) are not being acknowledged, so we finish them now
        onCommitAcknowledged(nonEventWrittenCount)
      }
      startWaitingForAcknowledgeTimer()
    }
  }

  private def startWaitingForAcknowledgeTimer(): Unit =
    if (requireClusterAcknowledgement && lastAcknowledgedEventId < lastWrittenEventId) {
      waitingForAcknowledgeSince = now
      waitingForAcknowledgeTimer := scheduler.scheduleAtFixedRates(conf.ackWarnDurations) {
        self ! Internal.StillWaitingForAcknowledge
      }
    }

  private def commitWithoutAcknowledgement(event: ClusterEvent): Unit = {
    if (requireClusterAcknowledgement) {
      logger.debug(s"No more acknowledgments required due to $event event")
      requireClusterAcknowledgement = false
      waitingForAcknowledgeTimer := Cancelable.empty
    }
    commit()
  }

  private def onCommitAcknowledged(n: Int): Unit = {
    finishCommitted(n)
    if (lastAcknowledgedEventId == lastWrittenEventId) {
      onAllCommitsFinished()
    }
  }

  private def finishCommitted(n: Int): Unit = {
    val ackWritten = persistBuffer.view.take(n)
    val standardPersistSeq = ackWritten collect { case o: StandardPersist => o }

    logCommitted(standardPersistSeq)

    for (lastFileLengthAndEventId <- ackWritten.flatMap(_.lastFileLengthAndEventId).lastOption) {
      lastAcknowledgedEventId = lastFileLengthAndEventId.value
      eventWriter.onCommitted(lastFileLengthAndEventId, n = ackWritten.map(_.eventCount).sum)
    }

    standardPersistSeq foreach updateStateAndContinueCallers

    // Update EventId for trailing acceptEarly events
    ackWritten.lastOption
      .collect { case o: AcceptEarlyPersist => o }
      .flatMap(_.lastFileLengthAndEventId)
      .foreach { case PositionAnd(_, eventId) =>
        committedState = committedState.withEventId(eventId)
      }

    persistBuffer.removeFirst(n)
    assertThat((lastAcknowledgedEventId == lastWrittenEventId) == persistBuffer.isEmpty)
  }

  private def onAllCommitsFinished(): Unit = {
    assertThat(lastAcknowledgedEventId == lastWrittenEventId)
    assertThat(persistBuffer.isEmpty)
    if (conf.slowCheckState && committedState != uncommittedState) {
      val msg = "JournaledState update mismatch: committedState != uncommittedState"
      logger.error(msg)
      logger.error(diffx.compare(committedState, uncommittedState).show)
      sys.error(msg)
    }
    uncommittedState = committedState    // Reduce duplicate allocated objects
    waitingForAcknowledgeTimer := Cancelable.empty
    maybeDoASnapshot()
  }

  private def updateStateAndContinueCallers(persist: StandardPersist): Unit = {
    committedState = committedState.applyStampedEvents(persist.stampedSeq)
      .orThrow/*may crash JournalActor !!!*/
    if (persist.replyTo != Actor.noSender) {
      // Continue caller
      reply(persist.sender, persist.replyTo, Output.Stored(persist.stampedSeq, committedState, persist.callersItem))
    }
    for (stamped <- persist.stampedSeq) {
      keyedEventBus.publish(stamped)
      handleJournalEvents(stamped)
    }
  }

  private def handleJournalEvents(stamped: Stamped[AnyKeyedEvent]): Unit =
    stamped.value match {
      case KeyedEvent(_: NoKey, SnapshotTaken) =>
        releaseObsoleteEvents()
        responseAfterSnapshotTaken()

      case KeyedEvent(_, _: JournalEventsReleased) =>
        releaseObsoleteEvents()

      case _ =>
    }

  private def maybeDoASnapshot(): Unit = {
    if (snapshotRequesters.isEmpty &&
      eventWriter.bytesWritten >= conf.snapshotSizeLimit &&
      fileEventCount >= 2 * committedState.estimatedSnapshotSize)
    {
      logger.debug(s"Take snapshot because written size ${toKBGB(eventWriter.bytesWritten)} is above the limit ${toKBGB(conf.snapshotSizeLimit)}")
      snapshotRequesters += self
    }
    tryTakeSnapshotIfRequested()  // TakeSnapshot has been delayed until last event has been acknowledged
    if (snapshotSchedule == null) {
      snapshotSchedule = scheduler.scheduleOnce(conf.snapshotPeriod) {
        if (!switchedOver) {
          self ! Input.TakeSnapshot
        }
      }
    }
  }

  private def reply(sender: ActorRef, replyTo: ActorRef, msg: Any): Unit =
    replyTo.!(msg)(sender)

  private def receiveGet: Receive = {
    case Input.GetJournalActorState =>
      sender() ! Output.JournalActorState(
        isFlushed = eventWriter != null && eventWriter.isFlushed,
        isSynced = eventWriter != null && eventWriter.isSynced,
        isRequiringClusterAcknowledgement = requireClusterAcknowledgement)

    case Input.GetJournaledState =>
      // Allow the caller outside of this JournalActor to read committedState
      // asynchronously at any time.
      sender() ! (() => committedState)
  }

  private val syncOrFlushString: String =
    if (conf.syncOnCommit) if (conf.simulateSync.isDefined) "~sync" else "sync "
    else "flush"

  private def logCommitted(persists: View[LoggablePersist]) =
    logger.whenTraceEnabled {
      val committedAt = now
      val iterator = dropLastEmptyPersists(persists).iterator
      while (iterator.hasNext) {
        val persist = iterator.next()
        logPersist(persist, isLastPersist = !iterator.hasNext, committedAt)
      }
    }

  private def dropLastEmptyPersists(persists: View[LoggablePersist]): View[LoggablePersist] = {
    var i, nonEmptyLength = 0
    for (o <- persists) {
      i += 1
      if (o.stampedSeq.nonEmpty) nonEmptyLength = i
    }
    persists take nonEmptyLength
  }

  private def logPersist(persist: LoggablePersist, isLastPersist: Boolean, committedAt: Deadline) = {
    var nr = persist.eventNumber
    var isFirst = true
    val stampedIterator = persist.stampedSeq.iterator
    while (stampedIterator.hasNext) {
      val stamped = stampedIterator.next()
      val isLast = !stampedIterator.hasNext
      val flushOrSync =
        if (isLast && persist.isLastOfFlushedOrSynced)
          syncOrFlushString
        else if (isFirst && persist.stampedSeq.lengthIs > 1)
          f"${persist.stampedSeq.length}%4d×"  // Neither flushed nor synced, and not the only event of a commit
        else
          "     "
      val a =
        if (!requireClusterAcknowledgement)
          ""        // No cluster. Caller may continue when flushed or synced
        else if (isLast && isLastPersist)
          " ACK"    // Last event of an acknowledged event bundle. Caller may continue
        else
          " ack"    // Event is part of an acknowledged event bundle
      val committed = if (isLast) "COMMITTED" else "committed"
      val t = if (isLast) ((committedAt - persist.since).msPretty + "     ") take 6 else "      "
      logger.trace(s"$committed #$nr $flushOrSync$a $t ${stamped.eventId} ${stamped.value.toString.takeWhile(_ != '\n').truncateWithEllipsis(200)}")
      nr += 1
      isFirst = false
    }
  }

  def tryTakeSnapshotIfRequested(): Unit =
    if (snapshotRequesters.nonEmpty) {
      if (lastWrittenEventId == lastSnapshotTakenEventId) {
        responseAfterSnapshotTaken()
      } else if (lastAcknowledgedEventId < lastWrittenEventId) {
        logger.debug(s"Delaying snapshot until last event has been committed and acknowledged (lastAcknowledgedEventId=$lastAcknowledgedEventId lastWrittenEventId=$lastWrittenEventId)")
      } else
        takeSnapshot()
        logReady()
    }

  private def responseAfterSnapshotTaken(): Unit = {
    for (sender <- snapshotRequesters if sender != self) sender ! Output.SnapshotTaken
    snapshotRequesters.clear()
  }

  private def takeSnapshot() = {
    val since = now
    val snapshotTaken = eventIdGenerator.stamp(KeyedEvent(JournalEvent.SnapshotTaken))

    if (snapshotSchedule != null) {
      snapshotSchedule.cancel()
      snapshotSchedule = null
    }
    if (eventWriter != null) {
      if (persistBuffer.nonEmpty) {  // Unfortunately we must avoid a recursion, because commit() may try a snapshot again
        commit()
      }
      closeEventWriter()
    }

    assertThat(journalHeader != null)
    journalHeader = journalHeader.nextGeneration(eventId = lastWrittenEventId, totalEventCount = totalEventCount,
      totalRunningTime = totalRunningSince.elapsed roundUpToNext 1.ms)
    val file = journalMeta.file(after = lastWrittenEventId)

    logger.info(s"Starting new journal file #${journalHeader.generation} '${file.getFileName}' with a snapshot")
    logger.debug(journalHeader.toString)

    snapshotWriter = new SnapshotJournalWriter(journalMeta, toSnapshotTemporary(file), after = lastWrittenEventId,
      simulateSync = conf.simulateSync)(scheduler)
    snapshotWriter.writeHeader(journalHeader)
    snapshotWriter.beginSnapshotSection()

    blocking {  // TODO Do not block the thread
      val builder = S.newBuilder()
      Await.result(
        committedState.toSnapshotObservable
          .filter {
            case SnapshotEventId(_) => false  // JournalHeader contains already the EventId
            case _  => true
          }
          .mapParallelOrderedBatch() { snapshotObject =>
            // TODO Crash with SerializationException like EventSnapshotWriter ?
            snapshotObject -> snapshotObject.asJson(S.snapshotObjectJsonCodec).toByteArray
          }
          .foreach { case (snapshotObject, byteArray) =>
            if (conf.slowCheckState) builder.addSnapshotObject(snapshotObject)
            snapshotWriter.writeSnapshot(byteArray)
            logger.trace(s"Snapshot ${snapshotObject.toString.truncateWithEllipsis(200)}")
          }(scheduler),
        999.s)

      if (conf.slowCheckState) {
        builder.onAllSnapshotsAdded()
        assertEqualSnapshotState("Written snapshot",
          builder.result().withEventId(journalHeader.eventId), Nil)
      }
    }

    snapshotWriter.endSnapshotSection()
    // Write a SnapshotTaken event to increment EventId and force a new filename
    snapshotWriter.beginEventSection(sync = false)
    val (fileLengthBeforeEvents, fileEventId) = (snapshotWriter.fileLength, lastWrittenEventId)

    snapshotWriter.writeEvent(snapshotTaken)
    snapshotWriter.flush(sync = conf.syncOnCommit)
    locally {
      val standardPersist = StandardPersist(totalEventCount + 1, snapshotTaken :: Nil, since,
        Some(PositionAnd(snapshotWriter.fileLength, snapshotTaken.eventId)), Actor.noSender, null, null)
      standardPersist.isLastOfFlushedOrSynced = true
      persistBuffer.add(standardPersist)
    }
    lastWrittenEventId = snapshotTaken.eventId
    uncommittedState = uncommittedState.applyStampedEvents(snapshotTaken :: Nil).orThrow
    if (!requireClusterAcknowledgement) {
      lastAcknowledgedEventId = lastWrittenEventId
    }
    totalEventCount += 1
    fileEventCount = 1
    lastSnapshotTakenEventId = snapshotTaken.eventId
    snapshotWriter.closeAndLog()

    move(snapshotWriter.file, journalMeta.file(after = fileEventId), ATOMIC_MOVE)
    snapshotWriter = null

    eventWriter = newEventJsonWriter(after = fileEventId)
    eventWriter.onJournalingStarted(fileLengthBeforeEvents = fileLengthBeforeEvents)

    onReadyForAcknowledgement()
  }

  private def assertEqualSnapshotState(what: String, snapshotState: S, stampedSeq: Seq[Stamped[AnyKeyedEvent]])
  : Unit =
    if (snapshotState != uncommittedState) {
      var msg = s"$what does not match actual '$S'"
      logger.error(msg)
      for (stamped <- stampedSeq) logger.error(stamped.toString.truncateWithEllipsis(200))
      // msg may get very big
      msg ++= s":\n" ++ diffx.compare(snapshotState, uncommittedState).show
      logger.info(msg)  // Without colors because msg is already colored
      throw new AssertionError(msg)
    }

  private def newEventJsonWriter(after: EventId, withoutSnapshots: Boolean = false) = {
    assertThat(journalHeader != null)
    val file = journalMeta.file(after = after)
    val w = new EventJournalWriter(journalMeta, file, after = after, journalHeader.journalId,
      journalingObserver.orThrow, simulateSync = conf.simulateSync,
      withoutSnapshots = withoutSnapshots, initialEventCount = 1/*SnapshotTaken*/)(scheduler)
    journalMeta.updateSymbolicLink(file)
    w
  }

  private def closeEventWriter(): Unit =
    if (eventWriter != null) {
      if (switchedOver) {
        eventWriter.flush(sync = conf.syncOnCommit)
        eventWriter.close()
      } else {
        eventWriter.closeProperly(sync = conf.syncOnCommit)
      }
      eventWriter = null
    }

  private def releaseObsoleteEvents(): Unit =
    if (conf.deleteObsoleteFiles) {
      if (committedState.clusterState == ClusterState.Empty ||
          requireClusterAcknowledgement
            // ClusterPassiveLost after SnapshotTaken in the same commit chunk has reset
            // requireClusterAcknowledgement. We must not delete the file when cluster is being decoupled.
          && (committedState.clusterState.isInstanceOf[ClusterState.Coupled] ||
              committedState.clusterState.isInstanceOf[ClusterState.ActiveShutDown])
          && releaseEventIdsAfterClusterCoupledAck.forall(_ <= lastAcknowledgedEventId))
      {
        val eventId =
          if (committedState.clusterState == ClusterState.Empty)
            lastWrittenEventId
          else {
            // Do not release the just acknowledged last Event of a journal file
            // (i.e. the last acknowledged event is the last event of a journal file)
            // because after restart, the passive node continues reading the journal file
            // (only to get end-of-file). Subtract 1 to avoid this.
            (lastAcknowledgedEventId - 1) max EventId.BeforeFirst
          }
        releaseObsoleteEventsUntil(committedState.journalState.toReleaseEventId(eventId, conf.releaseEventsUserIds))
        releaseEventIdsAfterClusterCoupledAck = None
      }
    }

  private def releaseObsoleteEventsUntil(untilEventId: EventId): Unit = {
    logger.debug(s"releaseObsoleteEvents($untilEventId) ${committedState.journalState}, clusterState=${committedState.clusterState}")
    journalingObserver.orThrow match {
      case Some(o) =>
        o.releaseEvents(untilEventId)(scheduler)
      case None =>
        // Without a JournalingObserver, we can delete all previous journal files (for Agent server)
        val until = untilEventId min journalHeader.eventId
        for (j <- listJournalFiles(journalFileBase = journalMeta.fileBase) if j.afterEventId < until) {
          val file = j.file
          assertThat(file != eventWriter.file)
          try delete(file)
          catch { case NonFatal(t) =>
            logger.warn(s"Cannot delete obsolete journal file '$file': ${t.toStringWithCauses}")
          }
        }
    }
  }
}

object JournalActor
{
  private val logger = Logger[this.type]
  private val TmpSuffix = ".tmp"  // Duplicate in PassiveClusterNode
  //private val DispatcherName = "js7.journal.dispatcher"  // Config setting; name is used for thread names

  //private val ClusterNodeHasBeenSwitchedOverProblem = Problem.pure("After switchover, this cluster node is no longer active")

  def props[S <: JournaledState[S]: JournaledState.Companion: diffx.Diff](
    journalMeta: JournalMeta,
    conf: JournalConf,
    keyedEventBus: StampedKeyedEventBus,
    scheduler: Scheduler,
    eventIdGenerator: EventIdGenerator,
    stopped: Promise[Stopped] = Promise())
  =
    Props {
      new JournalActor[S](journalMeta, conf, keyedEventBus, scheduler, eventIdGenerator, stopped)
    } //.withDispatcher(DispatcherName)

  private def toSnapshotTemporary(file: Path) = file.resolveSibling(s"${file.getFileName}$TmpSuffix")

  private[journal] trait CallersItem

  object Input {
    private[journal] final case class Start[S <: JournaledState[S]](
      journaledState: JournaledState[S],
      journalingObserver: Option[JournalingObserver],
      recoveredJournalHeader: JournalHeader,
      totalRunningSince: Deadline)
    final case class StartWithoutRecovery[S <: JournaledState[S]](
      journalId: JournalId,
      journalingObserver: Option[JournalingObserver] = None)
    private[journal] final case class Store(
      timestamped: Seq[Timestamped],
      journalingActor: ActorRef,
      acceptEarly: Boolean,
      transaction: Boolean,
      delay: FiniteDuration,
      alreadyDelayed: FiniteDuration,
      since: Deadline,
      callersItem: CallersItem)
    final case object TakeSnapshot
    final case class PassiveNodeAcknowledged(eventId: EventId)
    final case class PassiveLost(passiveLost: ClusterPassiveLost)
    final case object Terminate
    case object GetJournalActorState
    case object GetJournaledState
  }

  private[journal] trait Timestamped {
    def keyedEvent: AnyKeyedEvent
    def timestampMillis: Option[Long]
  }

  sealed trait Output
  object Output {
    final case class Ready(journalHeader: JournalHeader)

    private[journal] final case class Stored[S <: JournaledState[S]](
      stamped: Seq[Stamped[AnyKeyedEvent]],
      journaledState: S,
      callersItem: CallersItem)
    extends Output

    private[journal] final case class Accepted(callersItem: CallersItem) extends Output
    //final case class SerializationFailure(throwable: Throwable) extends Output
    final case class StoreFailure(problem: Problem, callersItem: CallersItem) extends Output
    final case object SnapshotTaken
    final case class JournalActorState(isFlushed: Boolean, isSynced: Boolean, isRequiringClusterAcknowledgement: Boolean)
  }

  type Stopped = Stopped.type
  final case object Stopped

  private object Internal {
    final case object Commit
    case object StillWaitingForAcknowledge extends DeadLetterSuppression
  }

  private class PersistBuffer {
    private val buffer = mutable.ArrayBuffer.empty[Persist]
    private var _eventCount = 0

    def add(persist: Persist): Unit = {
      buffer += persist
      persist match {
        case persist: StandardPersist => _eventCount += persist.stampedSeq.size
        case _ =>
      }
    }

    def removeFirst(n: Int): Unit = {
      buffer.view.take(n) foreach {
        case persist: StandardPersist => _eventCount -= persist.stampedSeq.size
        case _ =>
      }
      buffer.remove(0, n)
      assertThat(buffer.nonEmpty || eventCount == 0)
    }

    def isEmpty = buffer.isEmpty

    def nonEmpty = buffer.nonEmpty

    def size = buffer.size

    def view = buffer.view

    def iterator = buffer.iterator

    def eventCount = _eventCount
  }

  private sealed trait Persist {
    def eventCount: Int
    def isEmpty: Boolean
    def lastFileLengthAndEventId: Option[PositionAnd[EventId]]
    def lastStamped: Option[Stamped[AnyKeyedEvent]]
    def since: Deadline
  }

  private sealed trait LoggablePersist {
    def eventNumber: Long
    def stampedSeq: Seq[Stamped[AnyKeyedEvent]]
    def since: Deadline

    /** For logging: last stamped has been flushed */
    final var isLastOfFlushedOrSynced = false
  }

  /** A bundle of written but not yet committed (flushed and acknowledged) Persists. */
  private case class StandardPersist(
    eventNumber: Long,
    stampedSeq: Seq[Stamped[AnyKeyedEvent]],
    since: Deadline,
    lastFileLengthAndEventId: Option[PositionAnd[EventId]],
    replyTo: ActorRef,
    sender: ActorRef,
    callersItem: CallersItem)
  extends Persist with LoggablePersist
  {
    def isEmpty = stampedSeq.isEmpty

    def eventCount = stampedSeq.size

    def lastStamped: Option[Stamped[AnyKeyedEvent]] =
      stampedSeq.reverseIterator.buffered.headOption
  }

  // Without event to keep heap usage low (especially for many big stdout event)
  private case class AcceptEarlyPersist(eventCount: Int, since: Deadline, lastFileLengthAndEventId: Option[PositionAnd[EventId]], sender: ActorRef)
  extends Persist
  {
    def isEmpty = true
    def lastStamped = None
  }
}

package js7.core.event.journal

import akka.actor.{Actor, ActorRef, DeadLetterSuppression, Props, Stash, Terminated}
import akka.util.ByteString
import java.nio.file.Files.{delete, exists, move}
import java.nio.file.Path
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import js7.base.circeutils.CirceUtils._
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax.RichScheduler
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.RichThrowable
import js7.base.utils.SetOnce
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.utils.Strings.RichString
import js7.common.akkautils.Akkas.RichActorPath
import js7.common.akkautils.SupervisorStrategies
import js7.common.event.{EventIdGenerator, PositionAnd}
import js7.common.scalautil.Logger
import js7.common.utils.ByteUnits.toKBGB
import js7.core.event.StampedKeyedEventBus
import js7.core.event.journal.JournalActor._
import js7.core.event.journal.data.{JournalMeta, RecoveredJournalingActors}
import js7.core.event.journal.files.JournalFiles.{JournalMetaOps, listJournalFiles}
import js7.core.event.journal.watch.JournalingObserver
import js7.core.event.journal.write.{EventJournalWriter, ParallelExecutingPipeline, SnapshotJournalWriter}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.ClusterStateSnapshot
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, EventId, JournalEvent, JournalHeader, JournalId, JournalState, JournaledState, KeyedEvent, Stamped}
import monix.eval.Task
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Cancelable, Scheduler}
import org.scalactic.Requirements._
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, Duration, FiniteDuration}
import scala.concurrent.{Await, Promise, blocking}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class JournalActor[S <: JournaledState[S]] private(
  journalMeta: JournalMeta,
  conf: JournalConf,
  keyedEventBus: StampedKeyedEventBus,
  scheduler: Scheduler,
  eventIdGenerator: EventIdGenerator,
  stopped: Promise[Stopped],
  useJournaledStateAsSnapshot: Boolean)
extends Actor with Stash
{
  import context.{become, stop, watch}

  private val logger = Logger.withPrefix[this.type](journalMeta.fileBase.getFileName.toString)
  override val supervisorStrategy = SupervisorStrategies.escalate
  private val snapshotRequesters = mutable.Set[ActorRef]()
  private var snapshotSchedule: Cancelable = null

  private var uncommittedJournaledState: S = null.asInstanceOf[S]
  private var journaledState: S = null.asInstanceOf[S]

  /** Originates from `JournalValue`, calculated from recovered journal if not freshly initialized. */
  private var journalHeader: JournalHeader = null
  private var totalRunningSince = now
  private val journalingObserver = SetOnce[Option[JournalingObserver]]
  private var eventWriter: EventJournalWriter = null
  private var snapshotWriter: SnapshotJournalWriter = null
  private var lastSnapshotTakenEventId = EventId.BeforeFirst
  private val journalingActors = mutable.Set[ActorRef]()
  private val writtenBuffer = mutable.ArrayBuffer[Written]()
  private var lastWrittenEventId = EventId.BeforeFirst
  private var lastAcknowledgedEventId = EventId.BeforeFirst
  private var commitDeadline: Deadline = null
  private var delayedCommit: Cancelable = null
  private var totalEventCount = 0L
  private var requireClusterAcknowledgement = false
  private val waitingForAcknowledgeTimer = SerialCancelable()
  private var waitingForAcknowledgeSince = now
  private var switchedOver = false

  logger.debug(s"fileBase=${journalMeta.fileBase}")
  for (o <- conf.simulateSync) logger.warn(s"Disk sync is simulated with a ${o.pretty} pause")
  logger.whenTraceEnabled { logger.debug("Logger isTraceEnabled=true") }

  override def postStop() = {
    if (snapshotSchedule != null) snapshotSchedule.cancel()
    if (delayedCommit != null) delayedCommit.cancel()  // Discard commit for fast exit
    stopped.trySuccess(Stopped(keyedEventJournalingActorCount = journalingActors.size))
    if (!switchedOver) {
      for (a <- journalingActors) logger.debug(s"Journal stopped while a JournalingActor is still running: ${a.path.pretty}")
    }
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
    case Input.Start(journaledState_, RecoveredJournalingActors(keyToActor), observer_, header, totalRunningSince_) =>
      logger.debug(s"Thread ${Thread.currentThread.getName}")
      uncommittedJournaledState = journaledState_.asInstanceOf[S]
      journaledState = uncommittedJournaledState
      requireClusterAcknowledgement = journaledState.clusterState.isInstanceOf[ClusterState.Coupled]
      journalingObserver := observer_
      journalHeader = header
      totalRunningSince = totalRunningSince_
      lastWrittenEventId = header.eventId
      lastAcknowledgedEventId = header.eventId  // FIXME Möglicherweise ist die EventId noch nicht bestätigt ?  optional header.acknowledgedEventId ?
      totalEventCount = header.totalEventCount
      eventIdGenerator.updateLastEventId(lastWrittenEventId)
      if (!useJournaledStateAsSnapshot) {
        journalingActors ++= keyToActor.values
        journalingActors foreach watch
      }
      val sender = this.sender()
      locally {
        val file = toSnapshotTemporary(journalMeta.file(after = lastWrittenEventId))
        if (exists(file)) {
          logger.warn(s"JournalWriter: Deleting existent file '$file'")
          delete(file)
        }
      }
      becomeTakingSnapshotThen() { journalHeader =>
        unstashAll()
        becomeReady()
        sender ! Output.Ready(journalHeader)
      }

    case Input.StartWithoutRecovery(emptyState, observer_) =>  // Testing only
      uncommittedJournaledState = emptyState.asInstanceOf[S]
      journaledState = uncommittedJournaledState
      journalingObserver := observer_
      journalHeader = JournalHeader.initial(JournalId.random()).copy(generation = 1)
      eventWriter = newEventJsonWriter(after = EventId.BeforeFirst, withoutSnapshots = true)
      eventWriter.writeHeader(journalHeader)
      eventWriter.beginEventSection(sync = conf.syncOnCommit)
      eventWriter.onJournalingStarted()
      unstashAll()
      becomeReady()
      sender() ! Output.Ready(journalHeader)

    case Input.RegisterMe =>
      handleRegisterMe()

    case _ =>
      stash()
  }

  private def becomeReady(): Unit = {
    become(ready)
    logger.info(s"Ready, writing ${if (conf.syncOnCommit) "(with sync)" else "(without sync)"} journal file '${eventWriter.file.getFileName}'")
  }

  private def ready: Receive = receiveTerminatedOrGet orElse {
    case Input.RegisterMe =>
      handleRegisterMe()

    case Input.Store(timestamped, replyTo, acceptEarly, transaction, delay, alreadyDelayed, since, callersItem) =>
      if (switchedOver) {
        logger.warn(s"Event ignored while active cluster node is switching over: ${timestamped.headOption.map(_.keyedEvent)}")
        // We ignore the event and do not notify the caller,
        // because it would crash and disturb the process of switching-over.
        // (so AgentDriver with AgentReady event)
        // TODO The caller should handle the error (persist method does not allow this for now)
        //reply(sender(), replyTo, Output.StoreFailure(ClusterNodeHasBeenSwitchedOverProblem, callersItem))
      } else {
        val stampedEvents = timestamped.view.map(t => eventIdGenerator.stamp(t.keyedEvent, t.timestamp)).toVector
        uncommittedJournaledState.applyStampedEvents(stampedEvents) match {
          case Left(problem) =>
            logger.error(problem.toString)
            for (stamped <- stampedEvents) logger.error(stamped.toString)
            reply(sender(), replyTo, Output.StoreFailure(problem, callersItem))

          case Right(updatedState) =>
            uncommittedJournaledState = updatedState
            eventWriter.writeEvents(stampedEvents, transaction = transaction)
            val lastFileLengthAndEventId = stampedEvents.lastOption.map(o => PositionAnd(eventWriter.fileLength, o.eventId))
            for (o <- lastFileLengthAndEventId) {
              lastWrittenEventId = o.value
            }
            // TODO Handle serialization (but not I/O) error? writeEvents is not atomic.
            if (acceptEarly && !requireClusterAcknowledgement/*? irrelevant because acceptEarly is not used in a Cluster for now*/) {
              reply(sender(), replyTo, Output.Accepted(callersItem))
              writtenBuffer += AcceptEarlyWritten(stampedEvents.size, since, lastFileLengthAndEventId, sender())
              // acceptEarly-events will not update journaledState !!!
              // Ergibt falsche Reihenfolge mit dem anderen Aufruf: logCommitted(flushed = false, synced = false, stampedEvents)
            } else {
              writtenBuffer += NormallyWritten(totalEventCount + 1, stampedEvents, since, lastFileLengthAndEventId, replyTo, sender(), callersItem)
            }
            totalEventCount += stampedEvents.size
            stampedEvents.lastOption match {
              case Some(Stamped(_, _, KeyedEvent(_, _: ClusterCoupled))) =>
                // Commit now to let Coupled event take effect on following events (avoids deadlock)
                commit()
                logger.info(s"Cluster is coupled: Start requiring acknowledgements from passive cluster node")
                requireClusterAcknowledgement = true

              case Some(Stamped(_, _, KeyedEvent(_, _: ClusterSwitchedOver))) =>
                commit()
                logger.debug("SwitchedOver: no more events are accepted")
                switchedOver = true  // No more events are accepted

              case Some(Stamped(_, _, KeyedEvent(_, event: ClusterFailedOver))) =>
                commitWithoutAcknowledgement(event)

              case Some(Stamped(_, _, KeyedEvent(_, event: ClusterPassiveLost))) =>
                commitWithoutAcknowledgement(event)

              case _ => forwardCommit((delay max conf.delay) - alreadyDelayed)
            }
          }
      }

    case Internal.Commit(level) =>
      commitDeadline = null
      if (eventLimitReached)
        commit()
      else if (level < writtenBuffer.length) {
        // writtenBuffer has grown? Queue again to coalesce two commits (and flushs and maybe syncs)
        forwardCommit()
      } else if (level == writtenBuffer.length) {  // writtenBuffer has not grown since last emitted Commit
        commit()
      } else if (writtenBuffer.nonEmpty) {
        logger.trace(s"Commit($level) but writtenBuffer.length=${writtenBuffer.length}")
        commit()
      }

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
      // The passive node does not know Written bundles (maybe transactions) and acknowledges events as they arrive.
      // We take only complete Written bundles as acknowledged.
      onCommitAcknowledged(n = writtenBuffer.iterator.takeWhile(_.lastStamped.forall(_.eventId <= ack)).length)

    case Input.PassiveLost(passiveLost) =>
      // Side channel for Cluster to circumvent the ClusterEvent synchronization lock
      // in case of a concurrent open persist operation.
      // Must be followed by a ClusterPassiveLost event.
      commitWithoutAcknowledgement(passiveLost)

    case Input.Terminate =>
      logger.debug("Terminate")
      if (!switchedOver) {
        commit(terminating = true)
        closeEventWriter()
      }
      stop(self)

    case Input.AwaitAndTerminate =>  // For testing
      if (journalingActors.isEmpty) {
        closeEventWriter()
        stop(self)
      } else {
        become(receiveTerminatedOrGet andThen { _ =>
          if (journalingActors.isEmpty) {
            closeEventWriter()
            stop(self)
          }
        })
      }

    case Internal.StillWaitingForAcknowledge =>
      if (requireClusterAcknowledgement && lastAcknowledgedEventId < lastWrittenEventId) {
        val notAckSeq = writtenBuffer.collect { case o: LoggableWritten => o }
          .takeWhile(_.since.elapsed >= conf.ackWarnDurations.headOption.getOrElse(FiniteDuration.MaxValue))
        val n = writtenBuffer.map(_.eventCount).sum
        if (n > 0) {
          logger.warn(s"Since ${waitingForAcknowledgeSince.elapsed.pretty}" +
            s" waiting for acknowledgement from passive cluster node" +
            s" for $n events (in ${writtenBuffer.size} commits) starting with " +
            notAckSeq.flatMap(_.stampedSeq).headOption.fold("(unknown)")(_.toString.truncateWithEllipsis(200)) +
            s", lastAcknowledgedEventId=${EventId.toString(lastAcknowledgedEventId)}")
        } else logger.debug(s"StillWaitingForAcknowledge n=0, writtenBuffer.size=${writtenBuffer.size}")
      } else {
        waitingForAcknowledgeTimer := Cancelable.empty
      }
  }

  private def forwardCommit(delay: FiniteDuration = Duration.Zero): Unit = {
    val sender = context.sender()
    def commit = Internal.Commit(writtenBuffer.length)
    if (delay <= Duration.Zero)
      self.forward(commit)
    else {
      val deadline = now + delay
      if (commitDeadline == null || deadline < commitDeadline) {
        commitDeadline = deadline
        if (delayedCommit != null) delayedCommit.cancel()
        delayedCommit = scheduler.scheduleOnce(deadline.timeLeft) {
          self.tell(commit, sender)  // Don't  use forward in async operation
        }
      }
    }
  }

  /** Flushes and syncs the already written events to disk, then notifying callers and EventBus. */
  private def commit(terminating: Boolean = false): Unit = {
    commitDeadline = null
    if (writtenBuffer.nonEmpty) {
      try eventWriter.flush(sync = conf.syncOnCommit)
      catch { case NonFatal(t) if !terminating =>
        val tt = t.appendCurrentStackTrace
        //writtenBuffer foreach {
        //  case w: NormallyWritten => reply(sender(), w.replyTo, Output.StoreFailure(Problem.pure(tt), CALLERS_ITEM))  // TODO Failing flush is fatal
        //  case _: AcceptEarlyWritten =>  // TODO Error handling?
        //}
        throw tt;
      }
      for (w <- writtenBuffer.view.reverse collectFirst { case o: NormallyWritten => o }) {
        w.isLastOfFlushedOrSynced = true  // For logging: This last Written (including all before) has been flushed or synced
      }
      if (!terminating) {
        onReadyForAcknowledgement()
      }
    }
  }

  private def onReadyForAcknowledgement(): Unit = {
    if (!requireClusterAcknowledgement) {
      onCommitAcknowledged(writtenBuffer.length)
    } else {
      val nonEventWrittenCount = writtenBuffer.iterator.takeWhile(_.isEmpty).size
      if (nonEventWrittenCount > 0) {
        // `Written` without events (Nil) are not being acknowledged, so we finish them now
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
    val ackWritten = writtenBuffer.view.take(n)
    val normallyWritten = ackWritten collect { case o: NormallyWritten => o }

    logCommitted(ack = requireClusterAcknowledgement, normallyWritten.iterator)

    for (lastFileLengthAndEventId <- ackWritten.flatMap(_.lastFileLengthAndEventId).lastOption) {
      lastAcknowledgedEventId = lastFileLengthAndEventId.value
      eventWriter.onCommitted(lastFileLengthAndEventId, n = ackWritten.map(_.eventCount).sum)
    }

    normallyWritten foreach updateStateAndContinueCallers

    ackWritten.lastOption
      .collect { case o: AcceptEarlyWritten => o }
      .flatMap(_.lastFileLengthAndEventId)
      .foreach { case PositionAnd(_, eventId) =>
        journaledState = journaledState.withEventId(eventId)
      }

    writtenBuffer.remove(0, n)
    assertThat((lastAcknowledgedEventId == lastWrittenEventId) == writtenBuffer.isEmpty)
  }

  private def onAllCommitsFinished(): Unit = {
    assertThat(lastAcknowledgedEventId == lastWrittenEventId)
    assertThat(writtenBuffer.isEmpty)
    if (conf.slowCheckState) requireState(journaledState == uncommittedJournaledState)
    uncommittedJournaledState = journaledState    // Reduce duplicate allocated objects
    waitingForAcknowledgeTimer := Cancelable.empty
    maybeDoASnapshot()
  }

  private def updateStateAndContinueCallers(written: NormallyWritten): Unit = {
    journaledState = journaledState.applyStampedEvents(written.stampedSeq)
      .orThrow/*crashes JournalActor !!!*/
    if (written.replyTo != Actor.noSender) {
      // Continue caller
      reply(written.sender, written.replyTo, Output.Stored(written.stampedSeq, journaledState, written.callersItem))
    }
    for (stamped <- written.stampedSeq) {
      keyedEventBus.publish(stamped)
      handleJournalEvents(stamped)
    }
  }

  private def handleJournalEvents(stamped: Stamped[AnyKeyedEvent]): Unit = {
    stamped.value match {
      case KeyedEvent(_: NoKey, SnapshotTaken) =>
        releaseObsoleteEvents()
        responseAfterSnapshotTaken()

      case KeyedEvent(_, _: JournalEventsReleased) =>
        releaseObsoleteEvents()

      case _ =>
    }
  }

  private def maybeDoASnapshot(): Unit = {
    if (eventWriter.bytesWritten > conf.snapshotSizeLimit && snapshotRequesters.isEmpty) {  // Snapshot is not counted
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

  private def receiveTerminatedOrGet: Receive = receiveGet orElse {
    case Terminated(a) if journalingActors contains a =>
      onJournalingActorTerminated(a)

    case Terminated(a) =>
      // ??? Under JournalTest, Actor TEST-B, after removed, may send Terminated() twice since SnapshotTaker actor has been merged into JournalActor
      logger.error(s"Unknown actor has terminated: ${a.path.pretty}")
      //unhandled(msg)
  }

  private def receiveGet: Receive = {
    case Input.GetJournalActorState =>
      sender() ! Output.JournalActorState(
        isFlushed = eventWriter != null && eventWriter.isFlushed,
        isSynced = eventWriter != null && eventWriter.isSynced,
        isRequiringClusterAcknowledgement = requireClusterAcknowledgement)

    case Input.GetJournaledState =>
      sender() ! journaledState
  }

  private val syncOrFlushString: String =
    if (conf.syncOnCommit) if (conf.simulateSync.isDefined) "~sync" else "sync "
    else "flush"

  private def logCommitted(ack: Boolean, writtenIterator: Iterator[LoggableWritten]) =
    logger.whenTraceEnabled {
      val nw = now
      while (writtenIterator.hasNext) {
        val written = writtenIterator.next()
        var nr = written.eventNumber
        var firstInCommit = true
        val stampedIterator = written.stampedSeq.iterator
        while (stampedIterator.hasNext) {
          val stamped = stampedIterator.next()
          val n = written.stampedSeq.length
          val flushOrSync =
            if (written.isLastOfFlushedOrSynced && !stampedIterator.hasNext)
              syncOrFlushString
            else if (n > 1 && firstInCommit)
              f"${written.stampedSeq.length}%4d×"  // Neither flushed nor synced, and not the only event of a commit
            else
              "     "
          val a =
            if (!requireClusterAcknowledgement)
              ""        // No cluster. Caller may continue if flushed or synced
            else if (!ack)
              "    "    // Only SnapshotWritten event
            else if (writtenIterator.hasNext || stampedIterator.hasNext)
              " ack"    // Event is part of an acknowledged event bundle
            else
              " ACK"    // Last event of an acknowledged event bundle. Caller may continue
          val committed = if (stampedIterator.hasNext) "committed" else "COMMITTED"
          val t = if (stampedIterator.hasNext) "       " else ((nw - written.since).msPretty + "      ") take 7
          logger.trace(s"#$nr $flushOrSync$a $committed $t ${stamped.eventId} ${stamped.value.toString.takeWhile(_ != '\n')}")
          nr += 1
          firstInCommit = false
        }
      }
    }

  def tryTakeSnapshotIfRequested(): Unit =
    if (snapshotRequesters.nonEmpty) {
      if (lastWrittenEventId == lastSnapshotTakenEventId) {
        responseAfterSnapshotTaken()
      } else if (lastAcknowledgedEventId < lastWrittenEventId) {
        logger.debug(s"Delaying snapshot until last event has been committed and acknowledged (lastAcknowledgedEventId=$lastAcknowledgedEventId lastWrittenEventId=$lastWrittenEventId)")
      } else
        becomeTakingSnapshotThen() { _ =>
          becomeReady()
        }
    }

  private def responseAfterSnapshotTaken(): Unit = {
    for (sender <- snapshotRequesters if sender != self) sender ! Output.SnapshotTaken
    snapshotRequesters.clear()
  }

  private def becomeTakingSnapshotThen()(andThen: JournalHeader => Unit) = {
    val since = now
    val snapshotTaken = eventIdGenerator.stamp(KeyedEvent(JournalEvent.SnapshotTaken))

    if (snapshotSchedule != null) {
      snapshotSchedule.cancel()
      snapshotSchedule = null
    }
    if (eventWriter != null) {
      if (writtenBuffer.nonEmpty) {  // Unfortunately we must avoid a recursion, because commit() may try a snapshot again
        commit()
      }
      closeEventWriter()
    }

    assertThat(journalHeader != null)
    journalHeader = journalHeader.nextGeneration(eventId = lastWrittenEventId, totalEventCount = totalEventCount,
      totalRunningTime = totalRunningSince.elapsed roundUpToNext 1.ms)
    val file = journalMeta.file(after = lastWrittenEventId)
    logger.info(s"Starting new journal file #${journalHeader.generation} '${file.getFileName}' with a snapshot")
    snapshotWriter = new SnapshotJournalWriter(journalMeta, toSnapshotTemporary(file), after = lastWrittenEventId,
      simulateSync = conf.simulateSync)
    snapshotWriter.writeHeader(journalHeader)
    snapshotWriter.beginSnapshotSection()
    takeSnapshotNow(
      snapshotWriter,
      journalingActors.toSet,
      () => onSnapshotFinished(snapshotTaken, since, () => andThen(journalHeader)))
  }

  private def takeSnapshotNow(snapshotWriter: SnapshotJournalWriter, journalingActors: Set[ActorRef], andThen: () => Unit): Unit = {
    Await.result(
      journaledState.toSnapshotObservable
        .filter {
          case _ if useJournaledStateAsSnapshot => true
          case _: ClusterStateSnapshot | _: JournalState => true
          case _ => false
        }
        .mapParallelOrdered(sys.runtime.availableProcessors)(snapshotObject =>
          Task {
            // TODO Crash with SerializationException like EventSnapshotWriter
            val json = journalMeta.snapshotJsonCodec(snapshotObject)
            snapshotObject -> ByteString(json.compactPrint)
          })
        .foreach { case (snapshotObject, byteString) =>
          snapshotWriter.writeSnapshot(byteString)
          logger.trace(s"Snapshot $snapshotObject")
        }(scheduler),
      999.s)  // TODO Do not block the thread - Muss es ein Observable sein? Vielleicht LazyList oder Iterator?

    if (journalingActors.isEmpty) {
      andThen()
    } else {
      val snapshotRunningSince = now
      val logProgressCancelable = scheduler.scheduleWithFixedDelay(conf.snapshotLogProgressPeriod, conf.snapshotLogProgressPeriod) {
        self ! Internal.LogSnapshotProgress
      }
      val remaining = mutable.Set.empty ++ journalingActors
      val pipeline = new ParallelExecutingPipeline[ByteString](snapshotWriter.writeSnapshot)(scheduler)

      for (a <- journalingActors) {
        a ! JournalingActor.Input.GetSnapshot  // DeadLetter when actor just now terminates (a terminating JournalingActor must not have a snapshot)
      }
      become(receiveGet orElse {
        case JournalingActor.Output.GotSnapshot(snapshots) =>
          try blocking {  // blockingAdd blocks
            for (snapshot <- snapshots) {
              pipeline.blockingAdd { ByteString(journalMeta.snapshotJsonCodec(snapshot).compactPrint) }   // TODO Crash with SerializationException like EventSnapshotWriter
              logger.trace(s"Snapshot $snapshot")
            }
            onDone(sender())
          } catch { case NonFatal(t) =>
            logger.error(t.toStringWithCauses)
            throw t.appendCurrentStackTrace  // Crash JournalActor !!!
          }

        case Terminated(a) if journalingActors contains a =>
          onJournalingActorTerminated(a)
          if (remaining contains a) {
            logger.debug(s"${a.path.pretty} terminated while taking snapshot")
            onDone(a)
          }

        case Internal.LogSnapshotProgress =>
          val limit = remaining.size min conf.snapshotLogProgressActorLimit
          logger.info(s"Writing journal snapshot for ${snapshotRunningSince.elapsed.pretty}, ${remaining.size} snapshot elements remaining" +
            (if (limit == remaining.size) "" else s" (showing $limit actors)") +
            ":")
          for (o <- remaining take limit) {
            logger.info(s"... awaiting snapshot element from actor ${o.path.pretty}")
          }

        case _ => stash()
      })

      def onDone(actor: ActorRef): Unit = {
        remaining -= actor
        if (remaining.isEmpty) {
          logProgressCancelable.cancel()
          pipeline.flush()
          unstashAll()
          andThen()
        }
      }
    }
  }

  private def onSnapshotFinished(
    snapshotTaken: Stamped[KeyedEvent[JournalEvent.SnapshotTaken]],
    since: Deadline,
    andThen: () => Unit
  ): Unit = {
    snapshotWriter.endSnapshotSection()
    // Write a SnapshotTaken event to increment EventId and force a new filename
    snapshotWriter.beginEventSection(sync = false)
    val (fileLengthBeforeEvents, fileEventId) = (snapshotWriter.fileLength, lastWrittenEventId)

    // TODO Similar code
    snapshotWriter.writeEvent(snapshotTaken)
    writtenBuffer += NormallyWritten(totalEventCount + 1, snapshotTaken :: Nil, since,
      Some(PositionAnd(snapshotWriter.fileLength, snapshotTaken.eventId)), Actor.noSender, null, null)
    snapshotWriter.flush(sync = conf.syncOnCommit)
    lastWrittenEventId = snapshotTaken.eventId
    uncommittedJournaledState = uncommittedJournaledState.applyStampedEvents(snapshotTaken :: Nil).orThrow
    if (!requireClusterAcknowledgement) {
      lastAcknowledgedEventId = lastWrittenEventId  // We do not care for the acknowledgement of SnapshotTaken only, to ease shutdown ???
    }
    totalEventCount += 1
    lastSnapshotTakenEventId = snapshotTaken.eventId
    //logCommitted(ack = false,
    //  Iterator.single(LoggableWritten(totalEventCount + 1, snapshotTaken :: Nil, since, lastOfFlushedOrSynced = true)))
    snapshotWriter.closeAndLog()

    val file = journalMeta.file(after = fileEventId)
    move(snapshotWriter.file, file, ATOMIC_MOVE)
    snapshotWriter = null

    eventWriter = newEventJsonWriter(after = fileEventId)
    eventWriter.onJournalingStarted(fileLengthBeforeEvents = fileLengthBeforeEvents)

    onReadyForAcknowledgement()
    // Only when acknowledged: releaseObsoleteEvents()
    andThen()
  }

  private def onJournalingActorTerminated(a: ActorRef): Unit = {
    logger.trace(s"Terminated: ${a.path.pretty}")
    journalingActors -= a
  }

  private def newEventJsonWriter(after: EventId, withoutSnapshots: Boolean = false) = {
    assertThat(journalHeader != null)
    val file = journalMeta.file(after = after)
    val w = new EventJournalWriter(journalMeta, file, after = after, journalHeader.journalId,
      journalingObserver.orThrow, simulateSync = conf.simulateSync, withoutSnapshots = withoutSnapshots, initialEventCount = 1/*SnapshotTaken*/)
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
    if (conf.deleteObsoleteFiles &&
      (journaledState.clusterState == ClusterState.Empty ||
        (journaledState.clusterState.isInstanceOf[ClusterState.Coupled] ||
         journaledState.clusterState.isInstanceOf[ClusterState.CoupledActiveShutDown]) &&
          requireClusterAcknowledgement/*ClusterPassiveLost after SnapshotTaken in the same commit chunk
           has reset requireClusterAcknowledgement. We must not delete the file when cluster is being decoupled.*/))
    {
      releaseObsoleteEvents(journaledState.journalState.toReleaseEventId(lastAcknowledgedEventId, conf.releaseEventsUserIds))
    }

  private def releaseObsoleteEvents(untilEventId: EventId): Unit = {
    logger.debug(s"releaseObsoleteEvents($untilEventId) ${journaledState.journalState}, clusterState=${journaledState.clusterState}")
    journalingObserver.orThrow match {
      case Some(o) =>
        o.releaseEvents(untilEventId)
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

  private def handleRegisterMe() = {
    if (!useJournaledStateAsSnapshot) {
      journalingActors += sender()
      watch(sender())
    }
  }

  private def eventLimitReached: Boolean = {
    // Slow in Scala 2.13 (due to boxing?): writtenBuffer.iterator.map(_.eventCount) >= conf.eventLimit
    // TODO For even better performance, use an updated counter in a seperate class WrittenBuffer
    var remaining = conf.eventLimit
    val iterator = writtenBuffer.iterator
    while (remaining > 0 && iterator.hasNext) {
      remaining -= iterator.next().eventCount
    }
    remaining <= 0
  }
}

object JournalActor
{
  private val TmpSuffix = ".tmp"  // Duplicate in PassiveClusterNode
  private val DispatcherName = "js7.journal.dispatcher"  // Config setting; name is used for thread names

  //private val ClusterNodeHasBeenSwitchedOverProblem = Problem.pure("After switchover, this cluster node is no longer active")

  def props[S <: JournaledState[S]](
    journalMeta: JournalMeta,
    conf: JournalConf,
    keyedEventBus: StampedKeyedEventBus,
    scheduler: Scheduler,
    eventIdGenerator: EventIdGenerator,
    stopped: Promise[Stopped] = Promise(),
    useJournaledStateAsSnapshot: Boolean = false)
  =
    Props {
      new JournalActor[S](journalMeta, conf, keyedEventBus, scheduler, eventIdGenerator, stopped,
        useJournaledStateAsSnapshot || conf.useJournaledStateAsSnapshot)
    }.withDispatcher(DispatcherName)

  private def toSnapshotTemporary(file: Path) = file.resolveSibling(s"${file.getFileName}$TmpSuffix")

  private[journal] trait CallersItem

  object Input {
    private[journal] final case class Start[S <: JournaledState[S]](
      journaledState: JournaledState[S],
      recoveredJournalingActors: RecoveredJournalingActors,
      journalingObserver: Option[JournalingObserver],
      recoveredJournalHeader: JournalHeader,
      totalRunningSince: Deadline)
    final case class StartWithoutRecovery[S <: JournaledState[S]](emptyState: S, journalingObserver: Option[JournalingObserver] = None)
    /*private[journal] due to race condition when starting AgentDriver*/ case object RegisterMe
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
    final case object AwaitAndTerminate
    case object GetJournalActorState
    case object GetJournaledState
  }

  private[journal] trait Timestamped {
    def keyedEvent: AnyKeyedEvent
    def timestamp: Option[Timestamp]
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

  final case class Stopped(keyedEventJournalingActorCount: Int)

  private object Internal {
    final case class Commit(writtenLevel: Int)
    case object LogSnapshotProgress extends DeadLetterSuppression
    case object StillWaitingForAcknowledge extends DeadLetterSuppression
  }

  sealed trait Written {
    def eventCount: Int
    def isEmpty: Boolean
    def lastFileLengthAndEventId: Option[PositionAnd[EventId]]
    def lastStamped: Option[Stamped[AnyKeyedEvent]]
    def since: Deadline
  }

  private sealed trait LoggableWritten {
    def eventNumber: Long
    def stampedSeq: Seq[Stamped[AnyKeyedEvent]]
    def since: Deadline
    def isLastOfFlushedOrSynced: Boolean
  }

  /** A bundle of written but not yet committed (flushed and acknowledged) events. */
  private case class NormallyWritten(
    eventNumber: Long,
    stampedSeq: Seq[Stamped[AnyKeyedEvent]],
    since: Deadline,
    lastFileLengthAndEventId: Option[PositionAnd[EventId]],
    replyTo: ActorRef,
    sender: ActorRef,
    callersItem: CallersItem)
  extends Written with LoggableWritten
  {
    /** For logging: last stamped has been flushed */
    var isLastOfFlushedOrSynced = false

    def isEmpty = stampedSeq.isEmpty

    def eventCount = stampedSeq.size

    def lastStamped: Option[Stamped[AnyKeyedEvent]] =
      stampedSeq.reverseIterator.buffered.headOption
  }

  // Without event to keep heap usage low (especially for many big stdout event)
  private case class AcceptEarlyWritten(eventCount: Int, since: Deadline, lastFileLengthAndEventId: Option[PositionAnd[EventId]], sender: ActorRef)
  extends Written
  {
    def isEmpty = true
    def lastStamped = None
  }
}

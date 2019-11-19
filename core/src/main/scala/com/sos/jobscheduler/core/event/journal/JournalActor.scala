package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, ActorRef, DeadLetterSuppression, Props, Stash, Terminated}
import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.common.akkautils.Akkas.RichActorPath
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.event.{EventIdClock, EventIdGenerator, PositionAnd}
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.JournalActor._
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.{JournalMetaOps, listJournalFiles}
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.core.event.journal.write.{EventJournalWriter, ParallelExecutingPipeline, SnapshotJournalWriter}
import com.sos.jobscheduler.data.cluster.ClusterEvent
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, JournalEvent, JournalId, KeyedEvent, Stamped}
import java.nio.file.Files.{createSymbolicLink, delete, exists, move}
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.{Path, Paths}
import monix.execution.{Cancelable, Scheduler}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, Duration, FiniteDuration}
import scala.concurrent.{Promise, blocking}
import scala.util.Try
import scala.util.control.{NoStackTrace, NonFatal}

/**
  * @author Joacim Zschimmer
  */
final class JournalActor private(
  journalMeta: JournalMeta,
  runningSince: Deadline,
  conf: JournalConf,
  keyedEventBus: StampedKeyedEventBus,
  scheduler: Scheduler,
  eventIdClock: EventIdClock,
  stopped: Promise[Stopped])
extends Actor with Stash
{
  import context.{become, stop, watch}

  private val logger = Logger.withPrefix[this.type](journalMeta.fileBase.getFileName.toString)
  private val eventIdGenerator = new EventIdGenerator(eventIdClock)
  override val supervisorStrategy = SupervisorStrategies.escalate
  private var snapshotRequesters = mutable.Set[ActorRef]()
  private var snapshotSchedule: Cancelable = null

  /** Originates from `JournalValue`, calculated from recovered journal if not freshly initialized. */
  private var journalHeader: JournalHeader = null
  private var observer = SetOnce[Option[JournalingObserver]]
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
  private var waitingForAcknowledgeTimer = Cancelable.empty
  private var switchedOver = false

  logger.debug(s"fileBase=${journalMeta.fileBase}")
  for (o <- conf.simulateSync) logger.warn(s"Disk sync is simulated with a ${o.pretty} pause")

  override def postStop() = {
    if (snapshotSchedule != null) snapshotSchedule.cancel()
    if (delayedCommit != null) delayedCommit.cancel()  // Discard commit for fast exit
    stopped.trySuccess(Stopped(keyedEventJournalingActorCount = journalingActors.size))
    for (a <- journalingActors) logger.debug(s"Journal stopped while a JournalingActor is still running: ${a.path.pretty}")
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
    case Input.Start(RecoveredJournalingActors(keyToActor), observer_, header, requireClusterAcknowledgement) =>
      this.requireClusterAcknowledgement = requireClusterAcknowledgement
      observer := observer_
      journalHeader = header
      lastWrittenEventId = header.eventId
      lastAcknowledgedEventId = header.eventId
      totalEventCount = header.totalEventCount
      eventIdGenerator.updateLastEventId(lastWrittenEventId)
      journalingActors ++= keyToActor.values
      journalingActors foreach watch
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
        sender ! Output.Ready(journalHeader, runningSince)
      }

    case Input.StartWithoutRecovery(observer_) =>  // Testing only
      observer := observer_
      journalHeader = JournalHeader.initial(JournalId.random()).copy(generation = 1)
      eventWriter = newEventJsonWriter(after = EventId.BeforeFirst, withoutSnapshots = true)
      eventWriter.writeHeader(journalHeader)
      eventWriter.beginEventSection(sync = conf.syncOnCommit)
      eventWriter.onJournalingStarted()
      unstashAll()
      becomeReady()
      sender() ! Output.Ready(journalHeader, runningSince)

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

    case Input.Store(timestamped, replyTo, acceptEarly, transaction, delay, alreadyDelayed, callersItem) =>
      if (switchedOver) {
        logger.error(s"Event but journal has been switched over: ${timestamped.headOption.map(_.keyedEvent)}")
        reply(sender(), replyTo, Output.StoreFailure(new IllegalStateException("Journal has been switched over")))
      } else {
        val stampedEvents = for (t <- timestamped) yield eventIdGenerator.stamp(t.keyedEvent, t.timestamp)
        eventWriter.writeEvents(stampedEvents, transaction = transaction)
        val lastFileLengthAndEventId = stampedEvents.lastOption.map(o => PositionAnd(eventWriter.fileLength, o.eventId))
        for (o <- lastFileLengthAndEventId) {
          lastWrittenEventId = o.value
        }
        // TODO Handle serialization (but not I/O) error? writeEvents is not atomic.
        if (acceptEarly && !requireClusterAcknowledgement/*? irrelevant because acceptEarly is not used in a Cluster for now*/) {
          reply(sender(), replyTo, Output.Accepted(callersItem))
          writtenBuffer += AcceptEarlyWritten(stampedEvents.size, lastFileLengthAndEventId, sender())
          // Ergibt falsche Reihenfolge mit dem anderen Aufruf: logStored(flushed = false, synced = false, stampedEvents)
        } else {
          writtenBuffer += NormallyWritten(totalEventCount + 1, stampedEvents, lastFileLengthAndEventId, replyTo, sender(), callersItem)
        }
        totalEventCount += stampedEvents.size
        stampedEvents.lastOption match {
          case Some(Stamped(_, _, KeyedEvent(_, ClusterEvent.ClusterCoupled))) =>
            // Commit now to let ClusterCoupled event take effect on following events (avoids deadlock)
            commit()
            logger.info(s"ClusterCoupled: Start requiring acknowledges from passive cluster node")
            requireClusterAcknowledgement = true
          case Some(Stamped(_, _, KeyedEvent(_, _: ClusterEvent.SwitchedOver))) =>
            commit()
            logger.info(s"SwitchedOver: stopping journal")
            switchedOver = true  // No more events are accepted
            // Await acknowledge from other node
            // TODO Stop or restart JournalActor?
          case _ =>
            forwardCommit((delay max conf.delay) - alreadyDelayed)
        }
      }

    case Internal.Commit(level) =>
      commitDeadline = null
      if (writtenBuffer.iterator.map(_.eventCount).sum >= conf.eventLimit)
        commit()
      else if (level < writtenBuffer.length) {
        // writtenBuffer has grown? Queue again to coalesce two commits (and flushs and maybe syncs)
        forwardCommit()
      } else if (level == writtenBuffer.length) {  // writtenBuffer has not grown since last issued Commit
        commit()
      } else if (writtenBuffer.nonEmpty) {
        //logger.trace(s"Discarded: Commit($level), writtenBuffer.length=${writtenBuffer.length}")
        logger.trace(s"Commit($level) but writtenBuffer.length=${writtenBuffer.length}")
        commit()
      }

    case Input.TakeSnapshot if !switchedOver =>
      snapshotRequesters += sender()
      tryTakeSnapshotIfRequested()

    case Input.FollowerAcknowledged(eventId) =>
      onFollowerAcknowledged(eventId)

    case Input.Terminate =>
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
        val notAckSeq = writtenBuffer//.dropWhile(o => !o.lastStamped.map(_.eventId).contains(lastAcknowledgedEventId))
          .collect { case o: LoggableWritten => o }
        val n = writtenBuffer.map(_.eventCount).sum
        logger.warn(s"Still waiting for acknowledgement from passive cluster node for $n events starting with " +
          notAckSeq.flatMap(_.stamped).headOption.fold("(unknown)")(_.toString.truncateWithEllipsis((100))))
      } else {
        waitingForAcknowledgeTimer.cancel()
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
    try eventWriter.flush(sync = conf.syncOnCommit)
    catch { case NonFatal(t) if !terminating =>
      val tt = t.appendCurrentStackTrace
      writtenBuffer foreach {
        case w: NormallyWritten => reply(sender(), w.replyTo, Output.StoreFailure(tt))  // TODO Failing flush is fatal
        case _: AcceptEarlyWritten =>  // TODO Error handling?
      }
      throw tt;
    }
    for (w <- writtenBuffer.reverse collectFirst { case o: NormallyWritten => o }) {
      w.isLastOfFlushedOrSynced = true  // For logging: This last Written (with all before) has been flushed or synced
    }
    if (!terminating) {
      if (!requireClusterAcknowledgement) {
        onCommitAcknowledged(writtenBuffer.length)
      } else {
        val nonEventWrittenCount = writtenBuffer.takeWhile(_.isEmpty).size
        if (nonEventWrittenCount > 0) {
          // `Written` without events (Nil) are not being acknowledged, so we finish them now
          onCommitAcknowledged(nonEventWrittenCount)
        }
        startWaitingForAcknowledgeTimer()
      }
    }
  }

  private def startWaitingForAcknowledgeTimer(): Unit = {
    if (requireClusterAcknowledgement && lastAcknowledgedEventId < lastWrittenEventId) {
      waitingForAcknowledgeTimer.cancel()
      waitingForAcknowledgeTimer = scheduler.scheduleAtFixedRate(5.s, 10.s) {
        self ! Internal.StillWaitingForAcknowledge
      }
    }
  }

  private def onFollowerAcknowledged(eventId: EventId): Unit = {
    if (eventId > lastWrittenEventId) {
      val t = new RuntimeException(s"Passive cluster node acknowledged future event ${EventId.toString(eventId)}," +
        s" but lastWrittenEventId=${EventId.toString(lastWrittenEventId)}") with NoStackTrace
      logger.error(t.toString)
      sender() ! akka.actor.Status.Failure(t)
    } else {
      sender() ! Completed
      // The passive node does not know Written blocks (maybe transactions) and acknowledges events as they arrive.
      // We take only complete Written blocks as acknowledged.
      onCommitAcknowledged(n = writtenBuffer.iterator.takeWhile(_.lastStamped.forall(_.eventId <= eventId)).length)
    }
  }

  private def onCommitAcknowledged(n: Int): Unit = {
    val ackWritten = writtenBuffer take n
    logStored(ack = requireClusterAcknowledgement, ackWritten.iterator.collect { case o: NormallyWritten => o })
    for (lastFileLengthAndEventId <- ackWritten.flatMap(_.lastFileLengthAndEventId).lastOption) {
      lastAcknowledgedEventId = lastFileLengthAndEventId.value
      //val eventId = lastFileLengthAndEventId.value
      //if (requireClusterAcknowledgement) {
      //  logger.whenTraceEnabled {
      //    val i = totalEventCount - writtenBuffer.iterator.drop(ackWritten.length).map(_.eventCount).sum
      //    val duration = ackWritten.last.sinceStored match {
      //      case null => ""  // not flushed, no time measured
      //      case since => "+" + since.elapsed.pretty
      //    }
      //    logger.trace(f"#$i ack          $duration%-7s $eventId ${i - acknowledgedEventCount} events")
      //    acknowledgedEventCount = i
      //  }
      //}
      eventWriter.onCommitted(lastFileLengthAndEventId, n = ackWritten.iterator.map(_.eventCount).sum)
    }
    // AcceptEarlyWritten have already been replied.
    for (NormallyWritten(_, keyedEvents, _, replyTo, sender, item) <- ackWritten) {
      reply(sender, replyTo, Output.Stored(keyedEvents, item))
      keyedEvents foreach keyedEventBus.publish  // AcceptedEarlyWritten are not published !!!
    }
    writtenBuffer.remove(0, n)
    assertThat((lastAcknowledgedEventId == lastWrittenEventId) == writtenBuffer.isEmpty)
    if (lastAcknowledgedEventId == lastWrittenEventId) {
      waitingForAcknowledgeTimer.cancel()
    }
    maybeDoASnapshot()
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

  private def receiveTerminatedOrGet: Receive = {
    case Terminated(a) if journalingActors contains a =>
      onJournalingActorTerminated(a)

    case Terminated(a) =>
      // ??? Under JournalTest, Actor TEST-B, after removed, may send Terminated() twice since SnapshotTaker actor has been merged into JournalActor
      logger.error(s"Unknown actor has terminated: ${a.path.pretty}")
      //unhandled(msg)

    case Input.GetState =>
      sender() ! Output.State(
        isFlushed = eventWriter != null && eventWriter.isFlushed,
        isSynced = eventWriter != null && eventWriter.isSynced,
        isRequiringClusterAcknowledgement = requireClusterAcknowledgement)
  }

  private def logStored(ack: Boolean, writtenIterator: Iterator[LoggableWritten]) =
    logger.whenTraceEnabled {
      while (writtenIterator.hasNext) {
        val written = writtenIterator.next()
        var i = written.eventNumber
        val stampedIterator = written.stamped.iterator
        while (stampedIterator.hasNext) {
          val stamped = stampedIterator.next()
          val flushOrSync =
            if (!written.isLastOfFlushedOrSynced || stampedIterator.hasNext)
              "     "  // Wether flushed nor synced
            else if (conf.syncOnCommit) if (conf.simulateSync.isDefined)
              "~sync" else "sync "  // The file buffer has been flush and synced to disk
            else
              "flush"   // The file buffer has been flushed
          val a =
            if (!requireClusterAcknowledgement)
              ""  // No cluster. Caller may continue if flushed or synced
            else if (ack && !writtenIterator.hasNext && !stampedIterator.hasNext)
              " ack"  // Acknowledged by passive cluster node. Caller may continue
            else
              "    "
          logger.trace(f"#$i $flushOrSync$a STORED ${written.since.elapsed.pretty}%-7s ${stamped.eventId} ${stamped.value.toString.takeWhile(_ != '\n')}")
          i += 1
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
          responseAfterSnapshotTaken()
        }
    }

  private def responseAfterSnapshotTaken(): Unit = {
    for (sender <- snapshotRequesters if sender != self) sender ! Output.SnapshotTaken
    snapshotRequesters.clear()
  }

  private def becomeTakingSnapshotThen()(andThen: JournalHeader => Unit) = {
    val since = now
    val snapshotTaken = eventIdGenerator.stamp(KeyedEvent(JournalEvent.SnapshotTaken))
    val file = journalMeta.file(after = lastWrittenEventId)
    logger.info(s"Starting new journal file '${file.getFileName}' with a snapshot")

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
      totalRunningTime = journalHeader.totalRunningTime + runningSince.elapsed)
    snapshotWriter = new SnapshotJournalWriter(journalMeta, toSnapshotTemporary(file), after = lastWrittenEventId,
      simulateSync = conf.simulateSync)
    snapshotWriter.writeHeader(journalHeader)
    snapshotWriter.beginSnapshotSection()
    takeSnapshotNow(
      snapshotWriter,
      journalingActors.toSet,
      () => onSnapshotFinished(snapshotTaken, since, () => andThen(journalHeader)))
  }

  private def takeSnapshotNow(snapshotWriter: SnapshotJournalWriter, journalingActors: Set[ActorRef], andThen: () => Unit): Unit =
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
      context.become {
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
      }

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

  private def onSnapshotFinished(
    snapshotTaken: Stamped[KeyedEvent[JournalEvent.SnapshotTaken]],
    since: Deadline,
    andThen: () => Unit
  ): Unit = {
    snapshotWriter.endSnapshotSection()
    // Write a SnapshotTaken event to increment EventId and force a new filename
    snapshotWriter.beginEventSection(sync = false)
    val (fileLengthBeforeEvents, fileEventId) = (snapshotWriter.fileLength, lastWrittenEventId)

    snapshotWriter.writeEvent(snapshotTaken)
    lastSnapshotTakenEventId = snapshotTaken.eventId
    snapshotWriter.flush(sync = conf.syncOnCommit)
    lastWrittenEventId = snapshotTaken.eventId
    lastAcknowledgedEventId = lastWrittenEventId  // We do not care for the acknowledgement of SnapshotTaken only, to ease shutdown
    totalEventCount += 1
    logStored(ack = false,
      Iterator.single(LoggableWritten(totalEventCount + 1, snapshotTaken :: Nil, since, lastOfFlushedOrSynced = true)))
    snapshotWriter.closeAndLog()

    // Do not lock when SnapshotTaken is not being acknowledged: startWaitingForAcknowledgeTimer()

    val file = journalMeta.file(after = fileEventId)
    move(snapshotWriter.file, file, ATOMIC_MOVE)
    snapshotWriter = null

    eventWriter = newEventJsonWriter(after = fileEventId)
    eventWriter.onJournalingStarted(fileLengthBeforeEvents = fileLengthBeforeEvents)
    eventWriter.onInitialEventsWritten(eventId = lastWrittenEventId, n = 1)

    deleteObsoleteJournalFiles()
    andThen()
  }

  private def onJournalingActorTerminated(a: ActorRef): Unit = {
    logger.trace(s"Terminated: ${a.path.pretty}")
    journalingActors -= a
  }

  private def newEventJsonWriter(after: EventId, withoutSnapshots: Boolean = false) = {
    val symLink = Paths.get(journalMeta.fileBase + "-journal")  // We preserve the suffix ".journal" for the real journal files
    Try { if (exists(symLink)) delete(symLink) }

    val file = journalMeta.file(after = after)
    assertThat(journalHeader != null)
    val writer = new EventJournalWriter(journalMeta, file, after = after, journalHeader.journalId,
      observer.orThrow, simulateSync = conf.simulateSync, withoutSnapshots = withoutSnapshots, initialEventCount = 1/*SnapshotTaken*/)

    Try { createSymbolicLink(symLink, file.getFileName) }
    writer
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

  private def deleteObsoleteJournalFiles(): Unit =
    observer.orThrow match {
      case None =>
        for (file <- listJournalFiles(journalFileBase = journalMeta.fileBase) map (_.file) if file != eventWriter.file) {
          try delete(file)
          catch { case NonFatal(t) => logger.warn(s"Cannot delete file '$file': ${t.toStringWithCauses}") }
        }
      case Some(o) =>
        o.deleteObsoleteJournalFiles()
    }

  private def handleRegisterMe() = {
    journalingActors += sender()
    watch(sender())
  }
}

object JournalActor
{
  private val TmpSuffix = ".tmp"  // Duplicate in PassiveClusterNode
  private val DispatcherName = "jobscheduler.journal.dispatcher"  // Config setting; name is used for thread names

  def props[E <: Event](
    journalMeta: JournalMeta,
    runningSince: Deadline,
    conf: JournalConf,
    keyedEventBus: StampedKeyedEventBus,
    scheduler: Scheduler,
    eventIdClock: EventIdClock = EventIdClock.Default,
    stopped: Promise[Stopped] = Promise())
  =
    Props { new JournalActor(journalMeta, runningSince, conf, keyedEventBus, scheduler, eventIdClock, stopped) }
      .withDispatcher(DispatcherName)

  private def toSnapshotTemporary(file: Path) = file resolveSibling file.getFileName + TmpSuffix

  private[journal] trait CallersItem

  object Input {
    private[journal] final case class Start(
      recoveredJournalingActors: RecoveredJournalingActors,
      journalingObserver: Option[JournalingObserver],
      recoveredJournalHeader: JournalHeader,
      requireClusterAcknowledgement: Boolean)
    final case class StartWithoutRecovery(journalingObserver: Option[JournalingObserver] = None)
    /*private[journal] due to race condition when starting AgentDriver*/ case object RegisterMe
    private[journal] final case class Store(
      timestamped: Seq[Timestamped],
      journalingActor: ActorRef,
      acceptEarly: Boolean,
      transaction: Boolean,
      delay: FiniteDuration,
      alreadyDelayed: FiniteDuration,
      item: CallersItem)
    final case object TakeSnapshot
    final case class FollowerAcknowledged(eventId: EventId)
    final case object Terminate
    final case object AwaitAndTerminate
    case object GetState
  }

  private[journal] trait Timestamped {
    def keyedEvent: AnyKeyedEvent
    def timestamp: Option[Timestamp]
  }

  sealed trait Output
  object Output {
    final case class Ready(journalHeader: JournalHeader, runningSince: Deadline)
    private[journal] final case class Stored(stamped: Seq[Stamped[AnyKeyedEvent]], item: CallersItem) extends Output
    private[journal] final case class Accepted(item: CallersItem) extends Output
    //final case class SerializationFailure(throwable: Throwable) extends Output
    final case class StoreFailure(throwable: Throwable) extends Output
    final case object SnapshotTaken
    final case class State(isFlushed: Boolean, isSynced: Boolean, isRequiringClusterAcknowledgement: Boolean)
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

    val since = now
  }

  private sealed trait LoggableWritten {
    def eventNumber: Long
    def stamped: Seq[Stamped[AnyKeyedEvent]]
    def since: Deadline
    def isLastOfFlushedOrSynced: Boolean
  }
  private object LoggableWritten {
    def apply(number: Long, _stamped: Seq[Stamped[AnyKeyedEvent]], _since: Deadline, lastOfFlushedOrSynced: Boolean): LoggableWritten = {
      val x = lastOfFlushedOrSynced
      new LoggableWritten {
        def eventNumber = number
        def stamped = _stamped
        def since = _since
        def isLastOfFlushedOrSynced = x
      }
    }
  }

  private case class NormallyWritten(
    eventNumber: Long,
    stamped: Seq[Stamped[AnyKeyedEvent]],
    lastFileLengthAndEventId: Option[PositionAnd[EventId]],
    replyTo: ActorRef,
    sender: ActorRef,
    item: CallersItem)
  extends Written with LoggableWritten
  {
    /** For logging: last stamped has been flushed */
    var isLastOfFlushedOrSynced = false

    def isEmpty = stamped.isEmpty

    def eventCount = stamped.size

    def lastStamped: Option[Stamped[AnyKeyedEvent]] =
      stamped.reverseIterator.buffered.headOption
  }

  // Without event to keep heap usage low (especially for many big stdout event)
  private case class AcceptEarlyWritten(eventCount: Int, lastFileLengthAndEventId: Option[PositionAnd[EventId]], sender: ActorRef)
  extends Written
  {
    def isEmpty = true
    def lastStamped = None
  }
}

package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.akkautils.Akkas.{RichActorPath, uniqueActorName}
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.event.{EventIdClock, EventIdGenerator, PositionAnd}
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.JournalActor._
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.{JournalMetaOps, listJournalFiles}
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.core.event.journal.write.{EventJournalWriter, SnapshotJournalWriter}
import com.sos.jobscheduler.data.cluster.ClusterEvent
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, JournalEvent, JournalId, KeyedEvent, Stamped}
import java.nio.file.Files.{createSymbolicLink, delete, exists, move}
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.{Path, Paths}
import monix.execution.{Cancelable, Scheduler}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, Duration, FiniteDuration}
import scala.util.control.{NoStackTrace, NonFatal}
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class JournalActor private(
  journalMeta: JournalMeta,
  conf: JournalConf,
  keyedEventBus: StampedKeyedEventBus,
  scheduler: Scheduler,
  eventIdClock: EventIdClock,
  stopped: Promise[Stopped])
extends Actor with Stash
{
  import context.{actorOf, become, stop, watch}
  import journalMeta.snapshotJsonCodec

  private val logger = Logger.withPrefix[this.type](journalMeta.fileBase.getFileName.toString)
  private val runningSince = now
  private val eventIdGenerator = new EventIdGenerator(eventIdClock)
  override val supervisorStrategy = SupervisorStrategies.escalate
  private var snapshotRequested = false
  private var snapshotSchedule: Cancelable = null

  /** Originates from `JournalValue`, calculated from recovered journal if not freshly initialized. */
  private val recoveredJournalHeader = SetOnce[JournalHeader]
  private var observer = SetOnce[Option[JournalingObserver]]
  private var eventWriter: EventJournalWriter = null
  private var snapshotWriter: SnapshotJournalWriter = null
  private val journalingActors = mutable.Set[ActorRef]()
  private val writtenBuffer = mutable.ArrayBuffer[Written]()
  private var lastWrittenEventId = EventId.BeforeFirst
  private var commitDeadline: Deadline = null
  private var delayedCommit: Cancelable = null
  private var totalEventCount = 0L
  private var acknowledgedEventCount = 0L
  private var requireClusterAcknowledgement = false

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
    closeEventWriter()
    logger.debug("Stopped")
    super.postStop()
  }

  def receive = {
    case Input.Start(RecoveredJournalingActors(keyToActor), observer_, header) =>
      observer := observer_
      recoveredJournalHeader := header
      lastWrittenEventId = header.eventId
      totalEventCount = header.totalEventCount
      eventIdGenerator.updateLastEventId(lastWrittenEventId)
      journalingActors ++= keyToActor.values
      journalingActors foreach watch
      val sender = this.sender()
      locally {
        val file = toSnapshotTemporary(journalMeta.file(after = lastWrittenEventId))
        if (exists(file)) {
          logger.warn(s"JournalWriter: Deleting existent file '$file'")
          delete(file)  // TODO Provide alternative to move file
        }
      }
      becomeTakingSnapshotThen() { journalHeader =>
        unstashAll()
        becomeReady()
        sender ! Output.Ready(journalHeader, runningSince)
      }

    case Input.StartWithoutRecovery(observer_) =>  // Testing only
      observer := observer_
      val header = JournalHeader.initial(JournalId.random())
      recoveredJournalHeader := header
      eventWriter = newEventJsonWriter(after = EventId.BeforeFirst, withoutSnapshots = true)
      eventWriter.writeHeader(header)
      eventWriter.beginEventSection(sync = conf.syncOnCommit)
      eventWriter.onJournalingStarted()
      unstashAll()
      becomeReady()
      sender() ! Output.Ready(header, runningSince)

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
        writtenBuffer += NormallyWritten(stampedEvents, lastFileLengthAndEventId, replyTo, sender(), callersItem)
      }
      stampedEvents.lastOption match {
        case Some(Stamped(_, _, KeyedEvent(_, ClusterEvent.ClusterCoupled))) =>
          // Commit now to let ClusterCoupled event take effect on following events (avoids deadlock)
          commit()
          logger.info(s"ClusterCoupled: Start requiring acknowledges from passive cluster node")
          requireClusterAcknowledgement = true
        case _ =>
          forwardCommit((delay max conf.delay) - alreadyDelayed)
      }
      if (stampedEvents.nonEmpty) {
        scheduleNextSnapshot()
      }

    case Internal.Commit(level) =>
      commitDeadline = null
      if (writtenBuffer.iterator.map(_.eventCount).sum >= conf.eventLimit)
        commit()
      else if (level < writtenBuffer.length) {
        // writtenBuffer has grown? Queue again to coalesce two commits
        forwardCommit()
      } else if (level == writtenBuffer.length) {  // writtenBuffer has not grown since last issued Commit
        commit()
      } else if (writtenBuffer.nonEmpty) {
        logger.trace(s"Discarded: Commit($level), writtenBuffer.length=${writtenBuffer.length}")
      }
      if (eventWriter.bytesWritten > conf.snapshotSizeLimit && !snapshotRequested) {  // Snapshot is not counted
        logger.debug(s"Take snapshot because written size ${toKBGB(eventWriter.bytesWritten)} is above the limit ${toKBGB(conf.snapshotSizeLimit)}")
        requestSnapshot()
      }

    case Input.TakeSnapshot =>
      snapshotRequested = false
      if (!eventWriter.isEventWritten) {
        if (sender() != self) sender() ! Output.SnapshotTaken
      } else {
        val sender = this.sender()
        becomeTakingSnapshotThen() { _ =>
          becomeReady()
          if (sender != self) sender ! Output.SnapshotTaken
        }
      }

    case Input.FollowerAcknowledged(eventId) =>
      onFollowerAcknowledged(eventId)

    case Input.Terminate =>
      commit(terminating = true)
      stop(self)

    case Input.AwaitAndTerminate =>  // For testing
      if (journalingActors.isEmpty)
        stop(self)
      else
        become(receiveTerminatedOrGet andThen { _ =>
          if (journalingActors.isEmpty) {
            stop(self)
          }
        })
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
    logStored(flushed = true, synced = conf.syncOnCommit, writtenBuffer.iterator collect { case o: NormallyWritten => o })
    totalEventCount += writtenBuffer.iterator.map(_.eventCount).sum
    if (!terminating) {
      if (!requireClusterAcknowledgement)  {
        onCommitAcknowledged(writtenBuffer.length)
      } else {
        // TODO self ! timedout
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
      onCommitAcknowledged(n = writtenBuffer.takeWhile(_.lastStamped.forall(_.eventId <= eventId)).length)
    }
  }

  private def onCommitAcknowledged(n: Int): Unit = {
    for (lastFileLengthAndEventId <- writtenBuffer.take(n).flatMap(_.lastFileLengthAndEventId).lastOption) {
      if (requireClusterAcknowledgement) {
        val lastWritten = writtenBuffer(n - 1)
        val i = totalEventCount - writtenBuffer.iterator.drop(writtenBuffer.length - n + 1).map(_.eventCount).sum
        logger.trace(f"#$i Acknowledged ${lastWritten.since.elapsed.pretty}%-7s ${lastFileLengthAndEventId.value} ${i - acknowledgedEventCount} events")
        acknowledgedEventCount = i
      }
      eventWriter.onCommitted(lastFileLengthAndEventId, n = writtenBuffer.iterator.take(n).map(_.eventCount).sum)
    }
    // AcceptEarlyWritten have already been replied.
    for (NormallyWritten(keyedEvents, _, replyTo, sender, item) <- writtenBuffer take n) {
      reply(sender, replyTo, Output.Stored(keyedEvents, item))
      keyedEvents foreach keyedEventBus.publish  // AcceptedEarlyWritten are not published !!!
    }
    writtenBuffer.remove(0, n)
  }

  private def reply(sender: ActorRef, replyTo: ActorRef, msg: Any): Unit =
    replyTo.!(msg)(sender)

  private def receiveTerminatedOrGet: Receive = {
    case Terminated(a) if journalingActors contains a =>
      logger.trace(s"Terminated: ${a.path.pretty}")
      journalingActors -= a

    case Input.GetState =>
      sender() ! Output.State(
        isFlushed = eventWriter != null && eventWriter.isFlushed,
        isSynced = eventWriter != null && eventWriter.isSynced,
        isRequiringClusterAcknowledgement = requireClusterAcknowledgement)
  }

  private def logStored(flushed: Boolean, synced: Boolean, writtenIterator: Iterator[LoggableWritten]) =
    if (logger.underlying.isTraceEnabled) {
      var i = totalEventCount + 1
      while (writtenIterator.hasNext) {
        val written = writtenIterator.next()
        val stampedIterator = written.stamped.iterator
        while (stampedIterator.hasNext) {
          val stamped = stampedIterator.next()
          val last =
            if (stampedIterator.hasNext || writtenIterator.hasNext || !flushed && !synced) "     "
            else if (synced)
              if (conf.simulateSync.isDefined) "~sync" else "sync "
            else "flush"   // After the last one, the file buffer was flushed                 d
          logger.trace(f"#$i $last STORED ${written.elapsed.pretty}%-7s ${stamped.eventId} ${stamped.value}")
          i += 1
        }
      }
    }

  private def becomeTakingSnapshotThen()(andThen: JournalHeader => Unit) = {
    val snapshotTaken = eventIdGenerator.stamp(KeyedEvent(JournalEvent.SnapshotTaken))
    val file = journalMeta.file(after = lastWrittenEventId)
    logger.info(s"Starting new journal file '${file.getFileName}' with a snapshot")

    if (snapshotSchedule != null) {
      snapshotSchedule.cancel()
      snapshotSchedule = null
    }
    if (eventWriter != null) {
      commit()
      closeEventWriter()
    }

    val header = recoveredJournalHeader().update(eventId = lastWrittenEventId, totalEventCount = totalEventCount,
      totalRunningTime = recoveredJournalHeader().totalRunningTime + runningSince.elapsed)
    snapshotWriter = new SnapshotJournalWriter(journalMeta, toSnapshotTemporary(file), after = lastWrittenEventId,
      simulateSync = conf.simulateSync)
    snapshotWriter.writeHeader(header)
    snapshotWriter.beginSnapshotSection()
    actorOf(
      Props { new SnapshotTaker(snapshotWriter.writeSnapshot, journalingActors.toSet, snapshotJsonCodec, conf, scheduler) }
        .withDispatcher(DispatcherName),
      uniqueActorName("SnapshotTaker"))
    become(takingSnapshot(snapshotTaken, commander = sender(), () => andThen(header)))
  }

  private def takingSnapshot(snapshotTaken: Stamped[KeyedEvent[JournalEvent.SnapshotTaken]], commander: ActorRef, andThen: () => Unit): Receive = {
    case SnapshotTaker.Output.Finished(Failure(t)) =>
      throw t.appendCurrentStackTrace

    case SnapshotTaker.Output.Finished(Success(_/*snapshotCount*/)) =>
      snapshotWriter.endSnapshotSection()
      // Write a SnapshotTaken event to increment EventId and force a new filename
      snapshotWriter.beginEventSection(sync = false)
      val (fileLengthBeforeEvents, fileEventId) = (snapshotWriter.fileLength, lastWrittenEventId)

      snapshotWriter.writeEvent(snapshotTaken)
      snapshotWriter.flush(sync = conf.syncOnCommit)
      lastWrittenEventId = snapshotTaken.eventId
      totalEventCount += 1
      logStored(flushed = false, synced = false, Iterator(LoggableWritten(snapshotTaken :: Nil,
        Timestamp.now - snapshotTaken.timestamp/*wall-time, not real-time will be logged*/)))
      snapshotWriter.closeAndLog()

      val file = journalMeta.file(after = fileEventId)
      move(snapshotWriter.file, file, ATOMIC_MOVE)
      snapshotWriter = null

      eventWriter = newEventJsonWriter(after = fileEventId)
      eventWriter.onJournalingStarted(fileLengthBeforeEvents = fileLengthBeforeEvents)
      eventWriter.onInitialEventsWritten(eventId = lastWrittenEventId, n = 1)

      deleteObsoleteJournalFiles()
      unstashAll()
      andThen()

    case _ =>
      stash()
  }

  private def newEventJsonWriter(after: EventId, withoutSnapshots: Boolean = false) = {
    val symLink = Paths.get(journalMeta.fileBase + "-journal")  // We preserve the suffix ".journal" for the real journal files
    Try { if (exists(symLink)) delete(symLink) }

    val file = journalMeta.file(after = after)
    val writer = new EventJournalWriter(journalMeta, file, after = after, recoveredJournalHeader().journalId,
      observer(), simulateSync = conf.simulateSync, withoutSnapshots = withoutSnapshots, initialEventCount = 1/*SnapshotTaken*/)

    Try { createSymbolicLink(symLink, file.getFileName) }
    writer
  }

  def closeEventWriter(): Unit = {
    if (eventWriter != null) {
      eventWriter.closeProperly(sync = conf.syncOnCommit)
      eventWriter = null
    }
  }

  private def deleteObsoleteJournalFiles(): Unit =
    observer() match {
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

  private def scheduleNextSnapshot(): Unit =
    if (snapshotSchedule == null) {
      snapshotSchedule = scheduler.scheduleOnce(conf.snapshotPeriod) {
        requestSnapshot()
      }
    }

  private def requestSnapshot(): Unit = {
    if (!snapshotRequested) {
      snapshotRequested = true
      self ! Input.TakeSnapshot
    }
  }
}

object JournalActor
{
  private val TmpSuffix = ".tmp"  // Duplicate in PassiveClusterNode
  private val DispatcherName = "jobscheduler.journal.dispatcher"  // Config setting; name is used for thread names

  def props[E <: Event](
    journalMeta: JournalMeta,
    conf: JournalConf,
    keyedEventBus: StampedKeyedEventBus,
    scheduler: Scheduler,
    eventIdClock: EventIdClock = EventIdClock.Default,
    stopped: Promise[Stopped] = Promise())
  =
    Props { new JournalActor(journalMeta, conf, keyedEventBus, scheduler, eventIdClock, stopped) }
      .withDispatcher(DispatcherName)

  private def toSnapshotTemporary(file: Path) = file resolveSibling file.getFileName + TmpSuffix

  private[journal] trait CallersItem

  object Input {
    private[journal] final case class Start(
      recoveredJournalingActors: RecoveredJournalingActors,
      journalingObserver: Option[JournalingObserver],
      recoveredJournalHeader: JournalHeader)
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
  }

  sealed trait Written {
    def eventCount: Int
    def lastFileLengthAndEventId: Option[PositionAnd[EventId]]
    def lastStamped: Option[Stamped[AnyKeyedEvent]]
    val since = now
  }

  private sealed trait LoggableWritten {
    def stamped: Seq[Stamped[AnyKeyedEvent]]
    def elapsed: Duration
  }
  private object LoggableWritten {
    def apply(_stamped: Seq[Stamped[AnyKeyedEvent]], _elapsed: FiniteDuration) =
      new LoggableWritten {
        def stamped = _stamped
        def elapsed = _elapsed
      }
  }

  private case class NormallyWritten(
    stamped: Seq[Stamped[AnyKeyedEvent]],
    lastFileLengthAndEventId: Option[PositionAnd[EventId]],
    replyTo: ActorRef,
    sender: ActorRef,
    item: CallersItem)
  extends Written with LoggableWritten
  {
    def eventCount = stamped.size

    def lastStamped: Option[Stamped[AnyKeyedEvent]] =
      stamped.reverseIterator.buffered.headOption

    def elapsed = since.elapsed
  }

  // Without event to keep heap usage low (especially for many big stdout event)
  private case class AcceptEarlyWritten(eventCount: Int, lastFileLengthAndEventId: Option[PositionAnd[EventId]], sender: ActorRef) extends Written {
    def lastStamped = None
  }
}

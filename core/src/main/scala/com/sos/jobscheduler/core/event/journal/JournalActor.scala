package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.akkautils.Akkas.{RichActorPath, uniqueActorName}
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.event.{EventIdClock, EventIdGenerator}
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.JournalActor._
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.{JournalMetaOps, listJournalFiles}
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.core.event.journal.write.{EventJournalWriter, SnapshotJournalWriter}
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, JournalId, KeyedEvent, Stamped}
import java.nio.file.Files.{createSymbolicLink, delete, exists, move}
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.{Path, Paths}
import java.util.UUID.randomUUID
import monix.execution.{Cancelable, Scheduler}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, Duration, FiniteDuration}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class JournalActor[E <: Event] private(
  journalMeta: JournalMeta[E],
  conf: JournalConf,
  keyedEventBus: StampedKeyedEventBus,
  scheduler: Scheduler,
  eventIdClock: EventIdClock,
  stopped: Promise[Stopped])
extends Actor with Stash {

  import context.{actorOf, become, stop, watch}
  import journalMeta.snapshotJsonCodec

  private val logger = Logger.withPrefix[JournalActor[_]](journalMeta.fileBase.getFileName.toString)
  private val runningSince = now
  private val eventIdGenerator = new EventIdGenerator(eventIdClock)
  override val supervisorStrategy = SupervisorStrategies.escalate
  private var snapshotRequested = false
  private var snapshotSchedule: Cancelable = null

  /** Originates from `JournalValue`, caculcated from recovered journal if not freshly initialized. */
  private val recoveredJournalHeader = SetOnce[JournalHeader]
  private var observer = SetOnce[Option[JournalingObserver]]
  private var eventWriter: EventJournalWriter[E] = null
  private var snapshotWriter: SnapshotJournalWriter[E] = null
  private val journalingActors = mutable.Set[ActorRef]()
  private val writtenBuffer = mutable.ArrayBuffer[Written]()
  private var lastWrittenEventId = EventId.BeforeFirst
  private var commitDeadline: Deadline = null
  private var delayedCommit: Cancelable = null
  private var totalEventCount = 0L

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
      val header = JournalHeader.initial(JournalId(randomUUID))
      recoveredJournalHeader := header
      eventWriter = newEventJsonWriter(withoutSnapshots = true)
      eventWriter.writeHeader(header)
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
    eventWriter.beginEventSection()
  }

  private def ready: Receive = receiveTerminatedOrGet orElse {
    case Input.RegisterMe =>
      handleRegisterMe()

    case Input.Store(timestamped, replyTo, acceptEarly, transaction, delay, alreadyDelayed, callersItem) =>
      val stampedEvents = for (t <- timestamped) yield eventIdGenerator.stamp(t.keyedEvent.asInstanceOf[KeyedEvent[E]], t.timestamp)
      eventWriter.writeEvents(stampedEvents, transaction = transaction)
      for (lastStamped <- stampedEvents.lastOption) {
        lastWrittenEventId = lastStamped.eventId
      }
      // TODO Handle serialization (but not I/O) error? writeEvents is not atomic.
      if (acceptEarly) {
        reply(sender(), replyTo, Output.Accepted(callersItem))
        writtenBuffer += AcceptEarlyWritten(stampedEvents.size, sender())
        // Ergibt falsche Reihenfolge mit dem anderen Aufruf: logStored(flushed = false, synced = false, stampedEvents)
      } else {
        writtenBuffer += NormallyWritten(stampedEvents, replyTo, sender(), callersItem)
      }
      forwardCommit((delay max conf.delay) - alreadyDelayed)
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
          becomeReady()  // Writes EventHeader
          if (sender != self) sender ! Output.SnapshotTaken
        }
      }

    case Input.Terminate =>
      commit()
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
  private def commit(): Unit = {
    commitDeadline = null
    try eventWriter.flush(sync = conf.syncOnCommit)
    catch { case NonFatal(t) =>
      val tt = t.appendCurrentStackTrace
      writtenBuffer foreach {
        case w: NormallyWritten => reply(sender(), w.replyTo, Output.StoreFailure(tt))  // TODO Failing flush is fatal
        case _: AcceptEarlyWritten =>  // TODO Error handling?
      }
      throw tt;
    }
    logStored(flushed = true, synced = conf.syncOnCommit, writtenBuffer.iterator collect { case o: NormallyWritten => o } flatMap (_.stamped))
    for (NormallyWritten(keyedEvents, replyTo, sender, item) <- writtenBuffer) {
      reply(sender, replyTo, Output.Stored(keyedEvents, item))
      keyedEvents foreach keyedEventBus.publish  // AcceptedEarlyWritten are not published !!!
    }
    totalEventCount += writtenBuffer.iterator.map(_.eventCount).sum
    writtenBuffer.clear()
  }

  private def reply(sender: ActorRef, replyTo: ActorRef, msg: Any): Unit =
    replyTo.!(msg)(sender)

  private def receiveTerminatedOrGet: Receive = {
    case Terminated(a) if journalingActors contains a =>
      logger.trace(s"Terminated: ${a.path.pretty}")
      journalingActors -= a

    case Input.GetState =>
      sender() ! (
        if (eventWriter == null)
          Output.State(isFlushed = false, isSynced = false)
        else
          Output.State(isFlushed = eventWriter.isFlushed, isSynced = eventWriter.isSynced))
  }

  private def logStored(flushed: Boolean, synced: Boolean, stamped: TraversableOnce[Stamped[AnyKeyedEvent]]) =
    if (logger.underlying.isTraceEnabled) {
      val iterator = stamped.toIterator
      while (iterator.hasNext) {
        val stamped = iterator.next()
        val last = if (iterator.hasNext | !synced & !flushed) "     " else if (synced) "sync " else "flush"   // After the last one, the file buffer was flushed
        logger.trace(s"$last STORED ${stamped.eventId} ${stamped.value}")
      }
    }

  private def becomeTakingSnapshotThen()(andThen: JournalHeader => Unit) = {
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
    snapshotWriter = new SnapshotJournalWriter[E](journalMeta, toSnapshotTemporary(file), observer(), simulateSync = conf.simulateSync)
    snapshotWriter.writeHeader(header)
    snapshotWriter.beginSnapshotSection()
    actorOf(
      Props { new SnapshotTaker(snapshotWriter.writeSnapshot, journalingActors.toSet, snapshotJsonCodec, conf, scheduler) }
        .withDispatcher(DispatcherName),
      uniqueActorName("SnapshotTaker"))
    become(takingSnapshot(commander = sender(), () => andThen(header)))
  }

  private def takingSnapshot(commander: ActorRef, andThen: () => Unit): Receive = {
    case SnapshotTaker.Output.Finished(Failure(t)) =>
      throw t.appendCurrentStackTrace

    case SnapshotTaker.Output.Finished(Success(_/*snapshotCount*/)) =>
      snapshotWriter.endSnapshotSection(sync = conf.syncOnCommit)
      snapshotWriter.close()
      val file = journalMeta.file(after = lastWrittenEventId)
      move(snapshotWriter.file, file, ATOMIC_MOVE)
      snapshotWriter = null
      eventWriter = newEventJsonWriter()
      deleteObsoleteJournalFiles()
      unstashAll()
      andThen()

    case _ =>
      stash()
  }

  private def newEventJsonWriter(withoutSnapshots: Boolean = false) = {
    val symLink = Paths.get(journalMeta.fileBase + "-journal")  // We preserve the suffix ".journal" for the real journal files
    Try { if (exists(symLink)) delete(symLink) }

    val file = journalMeta.file(after = lastWrittenEventId)
    val writer = new EventJournalWriter[E](journalMeta, file, after = lastWrittenEventId, observer(), simulateSync = conf.simulateSync,
      withoutSnapshots = withoutSnapshots)

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
  private val TmpSuffix = ".tmp"
  private val DispatcherName = "jobscheduler.journal.dispatcher"  // Config setting; name is used for thread names

  def props[E <: Event](
    journalMeta: JournalMeta[E],
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
    final case object Terminate
    final case object AwaitAndTerminate
    private[journal] case object GetState
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
    private[journal] final case class State(isFlushed: Boolean, isSynced: Boolean)
  }

  final case class Stopped(keyedEventJournalingActorCount: Int)

  private object Internal {
    final case class Commit(writtenLevel: Int)
  }

  sealed trait Written {
    def eventCount: Int
  }

  private case class NormallyWritten(
    stamped: Seq[Stamped[AnyKeyedEvent]],  // None means no-operation (for deferAsync)
    replyTo: ActorRef,
    sender: ActorRef,
    item: CallersItem)
  extends Written
  {
    def eventCount = stamped.size

    def lastStamped: Option[Stamped[AnyKeyedEvent]] =
      stamped.reverseIterator.buffered.headOption
  }

  // Without event to keep heap usage low (especially for many big stdout event)
  private case class AcceptEarlyWritten(eventCount: Int, sender: ActorRef)
  extends Written
}

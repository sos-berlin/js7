package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.akkautils.Akkas.uniqueActorName
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.JournalActor._
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.{JournalMetaOps, listJournalFiles}
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.core.event.journal.write.{EventJournalWriter, SnapshotJournalWriter}
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import com.typesafe.config.Config
import java.nio.file.Files.{createSymbolicLink, delete, exists, move}
import java.nio.file.Paths
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import monix.execution.{Cancelable, Scheduler}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Promise
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class JournalActor[E <: Event] private(
  journalMeta: JournalMeta[E],
  config: Config,
  keyedEventBus: StampedKeyedEventBus,
  scheduler: Scheduler,
  stopped: Promise[Stopped],
  eventIdGenerator: EventIdGenerator)
extends Actor with Stash {

  import context.{actorOf, become, stop, watch}
  import journalMeta.snapshotJsonCodec

  private val logger = Logger.withPrefix[JournalActor[_]](journalMeta.fileBase.getFileName.toString)
  override val supervisorStrategy = SupervisorStrategies.escalate
  private val syncOnCommit = config.getBoolean("jobscheduler.journal.sync")
  private val simulateSync = config.durationOption("jobscheduler.journal.simulate-sync") map (_.toFiniteDuration)
  private val experimentalDelay = config.getDuration("jobscheduler.journal.delay").toFiniteDuration
  private val snapshotPeriod = config.ifPath("jobscheduler.journal.snapshot.period")(p ⇒ config.getDuration(p).toFiniteDuration)
  private val eventLimit = config.as[Int]("jobscheduler.journal.event-buffer-size")  // TODO Better limit byte count to avoid OutOfMemoryError
  private var snapshotCancelable: Cancelable = null

  private var observerOption: Option[JournalingObserver] = None
  private var eventWriter: EventJournalWriter[E] = null
  private var snapshotWriter: SnapshotJournalWriter[E] = null
  private val journalingActors = mutable.Set[ActorRef]()
  private val writtenBuffer = mutable.ArrayBuffer[Written]()
  private var lastWrittenEventId = EventId.BeforeFirst
  private var dontSync = true
  private var delayedCommit: Cancelable = null
  private var totalEventCount = 0L

  for (o ← simulateSync) logger.warn(s"sync is simulated with $o duration")

  override def postStop() = {
    if (snapshotCancelable != null) snapshotCancelable.cancel()
    if (delayedCommit != null) delayedCommit.cancel()  // Discard commit for fast exit
    stopped.trySuccess(Stopped(keyedEventJournalingActorCount = journalingActors.size))
    for (a ← journalingActors) logger.debug(s"Journal stopped while a JournalingActor is still running: ${a.path}")
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
    case Input.Start(RecoveredJournalingActors(keyToActor), observer, lastEventId, totalEventCount_) ⇒
      observerOption = observer
      lastWrittenEventId = lastEventId
      totalEventCount = totalEventCount_
      eventIdGenerator.updateLastEventId(lastEventId)
      journalingActors ++= keyToActor.values
      journalingActors foreach watch
      val sender = this.sender()
      locally {
        val file = journalMeta.file(after = lastEventId, extraSuffix = TmpSuffix)
        if (exists(file)) {
          logger.warn(s"JournalWriter: Deleting existent file '$file'")
          delete(file)  // TODO Provide alternative to move file
        }
      }
      becomeTakingSnapshotThen() {
        unstashAll()
        becomeReady()
        sender ! Output.Ready
      }

    case Input.StartWithoutRecovery ⇒  // Testing only
      eventWriter = newEventJsonWriter(withoutSnapshots = true)
      eventWriter.writeHeader(JournalHeader(eventId = lastWrittenEventId, totalEventCount = totalEventCount))
      unstashAll()
      becomeReady()
      sender() ! Output.Ready

    case Input.RegisterMe ⇒
      handleRegisterMe()

    case _ ⇒
      stash()
  }

  private def becomeReady(): Unit = {
    become(ready)
    logger.info(s"Ready, writing ${if (syncOnCommit) "(with sync)" else "(without sync)"} journal file '${eventWriter.file.getFileName}'")
    eventWriter.beginEventSection()
  }

  private def ready: Receive = receiveTerminatedOrGet orElse {
    case Input.RegisterMe ⇒
      handleRegisterMe()

    case Input.Store(timestamped, replyTo, noSync, transaction, item) ⇒
      val stampedEvents = for (t ← timestamped) yield eventIdGenerator.stamp(t.keyedEvent.asInstanceOf[KeyedEvent[E]], t.timestamp)
      /*try*/ eventWriter.writeEvents(stampedEvents, transaction = transaction)
      // TODO Handle serialization (but not I/O) error? writeEvents is not atomic.
      //catch {
      //  //case t: JournalWriter.SerializationException ⇒
      //  //  logger.error(t.getCause.toStringWithCauses, t.getCause)
      //  //  replyTo.forward(Output.SerializationFailure(t.getCause))  // TODO Handle message in JournaledActor
      //  case NonFatal(t) ⇒
      //    val tt = t.appendCurrentStackTrace
      //    logger.error(tt.toStringWithCauses, tt)
      //    replyTo.forward(Output.StoreFailure(tt))  // TODO Handle message in JournaledActor
      //    throw tt  // Stop Actor
      //}
      writtenBuffer += Written(stampedEvents, replyTo, sender(), item)
      dontSync &= noSync
      forwardCommit()
      if (stampedEvents.nonEmpty) {
        scheduleNextSnapshot()
      }

    case Internal.Commit(level) ⇒
      if (writtenBuffer.length >= eventLimit)
        commit()
      else if (level < writtenBuffer.length) {
        // writtenBuffer has grown? Queue again to coalesce two commits
        forwardCommit()
      } else if (level == writtenBuffer.length) {  // writtenBuffer has not grown since last issued Commit
        commit()
      } else if (writtenBuffer.nonEmpty) {
        logger.trace(s"Discarded: Commit($level), writtenBuffer.length=${writtenBuffer.length}")
      }

    case Input.TakeSnapshot ⇒
      if (!eventWriter.isEventWritten) {
        sender ! Output.SnapshotTaken
      } else {
        val sender = this.sender()
        becomeTakingSnapshotThen() {
          becomeReady()  // Writes EventHeader
          sender ! Output.SnapshotTaken
        }
      }

    case Output.SnapshotTaken ⇒  // In case, JournalActor itself has sent TakeSnapshot

    case Input.Terminate ⇒
      commit()
      stop(self)

    case Input.AwaitAndTerminate ⇒  // For testing
      if (journalingActors.isEmpty)
        stop(self)
      else
        become(receiveTerminatedOrGet andThen { _ ⇒
          if (journalingActors.isEmpty) {
            stop(self)
          }
        })
  }

  private def forwardCommit(): Unit = {
    val commit = Internal.Commit(writtenBuffer.length)
    if (experimentalDelay.isZero)
      self.forward(commit)
    else {
      if (delayedCommit != null) delayedCommit.cancel()
      delayedCommit = scheduler.scheduleOnce(experimentalDelay) {
        self.forward(commit)
      }
    }
  }

  /** Flushes and syncs the already written events to disk, then notifying callers and EventBus. */
  private def commit(): Unit = {
    if (delayedCommit != null) delayedCommit.cancel()
    val sync = syncOnCommit && !dontSync
    try eventWriter.flush(sync = sync)
    catch { case NonFatal(t) ⇒
      val tt = t.appendCurrentStackTrace
      for (w ← writtenBuffer) w.replyTo.!(Output.StoreFailure(tt))(sender)
      throw tt;
    }
    logWrittenAsStored(sync)
    for (Written(keyedEvents, replyTo, sender, item) ← writtenBuffer) {
      replyTo.!(Output.Stored(keyedEvents, item))(sender)
      for (lastStamped ← keyedEvents.lastOption) {
        lastWrittenEventId = lastStamped.eventId
      }
      keyedEvents foreach keyedEventBus.publish
    }
    totalEventCount += writtenBuffer.iterator.map(_.stamped.size).sum
    writtenBuffer.clear()
    dontSync = true
  }

  private def receiveTerminatedOrGet: Receive = {
    case Terminated(a) if journalingActors contains a ⇒
      logger.trace(s"Terminated: ${a.path}")
      journalingActors -= a

    case Input.GetState ⇒
      sender() ! (
        if (eventWriter == null)
          Output.State(isFlushed = false, isSynced = false)
        else
          Output.State(isFlushed = eventWriter.isFlushed, isSynced = eventWriter.isSynced))
  }

  private def logWrittenAsStored(synced: Boolean) =
    if (logger.underlying.isTraceEnabled) {
      val it = writtenBuffer.iterator flatMap (_.stamped)
      while (it.hasNext) {
        val stamped = it.next()
        val last = if (it.hasNext) "     " else if (synced) "sync " else "flush"  // After the last one, the file buffer was flushed
        logger.trace(s"$last STORED ${stamped.eventId} ${stamped.value}")
      }
    }

  private def becomeTakingSnapshotThen()(andThen: ⇒ Unit) = {
    logger.info(s"Starting new journal file with a snapshot")

    if (snapshotCancelable != null) {
      snapshotCancelable.cancel()
      snapshotCancelable = null
    }
    if (eventWriter != null) {
      commit()
      closeEventWriter()
    }

    snapshotWriter = new SnapshotJournalWriter[E](journalMeta,
      journalMeta.file(after = lastWrittenEventId, extraSuffix = TmpSuffix),
      observerOption, simulateSync = simulateSync)
    snapshotWriter.writeHeader(JournalHeader(eventId = lastWrittenEventId, totalEventCount = totalEventCount))
    snapshotWriter.beginSnapshotSection()
    actorOf(
      Props { new SnapshotTaker(snapshotWriter.writeSnapshot, journalingActors.toSet, snapshotJsonCodec, config, scheduler) },
      uniqueActorName("SnapshotTaker"))
    become(takingSnapshot(commander = sender(), () ⇒ andThen, new Stopwatch))
  }

  private def takingSnapshot(commander: ActorRef, andThen: () ⇒ Unit, stopwatch: Stopwatch): Receive = {
    case SnapshotTaker.Output.Finished(Failure(t)) ⇒
      throw t.appendCurrentStackTrace

    case SnapshotTaker.Output.Finished(Success(snapshotCount)) ⇒
      snapshotWriter.endSnapshotSection(sync = syncOnCommit)
      snapshotWriter.close()
      if (stopwatch.duration >= 1.s) logger.debug(stopwatch.itemsPerSecondString(snapshotCount, "snapshots") + " written")
      val file = journalMeta.file(after = lastWrittenEventId)
      move(snapshotWriter.file, file, ATOMIC_MOVE)
      snapshotWriter = null
      eventWriter = newEventJsonWriter()
      deleteObsoleteJournalFiles()
      unstashAll()
      andThen()

    case _ ⇒
      stash()
  }

  private def newEventJsonWriter(withoutSnapshots: Boolean = false) = {
    val symLink = Paths.get(journalMeta.fileBase + ".journal")
    Try { if (exists(symLink)) delete(symLink) }

    val file = journalMeta.file(after = lastWrittenEventId)
    val writer = new EventJournalWriter[E](journalMeta, file, after = lastWrittenEventId, observerOption, simulateSync = simulateSync,
      withoutSnapshots = withoutSnapshots)

    Try { createSymbolicLink(symLink, file.getFileName) }
    writer
  }

  def closeEventWriter(): Unit = {
    if (eventWriter != null) {
      eventWriter.closeProperly(sync = syncOnCommit)
      eventWriter = null
    }
  }

  private def deleteObsoleteJournalFiles(): Unit =
    observerOption match {
      case None ⇒
        for (file ← listJournalFiles(journalFileBase = journalMeta.fileBase) map (_.file) if file != eventWriter.file) {
          try delete(file)
          catch { case NonFatal(t) ⇒ logger.warn(s"Cannot delete file '$file': ${t.toStringWithCauses}") }
        }
      case Some(observer) ⇒
        observer.deleteObsoleteJournalFiles()
    }

  private def handleRegisterMe() = {
    journalingActors += sender()
    watch(sender())
  }

  private def scheduleNextSnapshot(): Unit =
    if (snapshotCancelable == null) {
      for (period ← snapshotPeriod) {
        snapshotCancelable = scheduler.scheduleOnce(period) {
          self ! Input.TakeSnapshot
        }
      }
    }
}

object JournalActor
{
  private val TmpSuffix = ".tmp"

  def props[E <: Event](
    journalMeta: JournalMeta[E],
    config: Config,
    keyedEventBus: StampedKeyedEventBus,
    scheduler: Scheduler,
    stopped: Promise[Stopped] = Promise(),
    eventIdGenerator: EventIdGenerator = new EventIdGenerator)
  =
    Props { new JournalActor(journalMeta, config, keyedEventBus, scheduler, stopped, eventIdGenerator) }

  private[journal] trait CallersItem

  object Input {
    private[journal] final case class Start(
      recoveredJournalingActors: RecoveredJournalingActors,
      journalingObserver: Option[JournalingObserver],
      lastEventId: EventId,
      totalEventCount: Long)
    final case object StartWithoutRecovery
    private[journal] case object RegisterMe
    private[journal] final case class Store(
      timestamped: Seq[Timestamped],
      journalingActor: ActorRef,
      noSync: Boolean,
      transaction: Boolean,
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
    final case object Ready
    private[journal] final case class Stored(stamped: Seq[Stamped[AnyKeyedEvent]], item: CallersItem) extends Output
    //final case class SerializationFailure(throwable: Throwable) extends Output
    final case class StoreFailure(throwable: Throwable) extends Output
    final case object SnapshotTaken
    private[journal] final case class State(isFlushed: Boolean, isSynced: Boolean)
  }

  final case class Stopped(keyedEventJournalingActorCount: Int)

  private object Internal {
    final case class Commit(writtenLevel: Int)
  }

  private case class Written(
    stamped: Seq[Stamped[AnyKeyedEvent]],  // None means no-operation (for deferAsync)
    replyTo: ActorRef,
    sender: ActorRef,
    item: CallersItem)
  {
    def lastStamped: Option[Stamped[AnyKeyedEvent]] =
      stamped.reverseIterator.buffered.headOption
  }
}

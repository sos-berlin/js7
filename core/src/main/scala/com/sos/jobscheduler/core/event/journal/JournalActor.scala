package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Problem
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
import com.sos.jobscheduler.core.event.journal.data.{JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.event.journal.write.JournalWriter
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import com.typesafe.config.Config
import java.nio.file.Files.move
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.{Files, Path}
import monix.execution.{Cancelable, Scheduler}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Promise
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

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

  private var journalWriter: JournalWriter[E] = null
  private var eventWatchOption: Option[JournalEventWatch[E]] = None
  private var temporaryJournalWriter: JournalWriter[E] = null
  private val journalingActors = mutable.Set[ActorRef]()
  private val writtenBuffer = mutable.ArrayBuffer[Written]()       // TODO Avoid OutOfMemoryError and commit when written JSON becomes big
  private var lastWrittenEventId = EventId.BeforeFirst
  private var dontSync = true
  private var delayedCommit: Cancelable = null

  for (o ← simulateSync) logger.warn(s"sync is simulated with $o duration")

  override def postStop() = {
    if (delayedCommit != null) delayedCommit.cancel()  // Discard commit for fast exit
    stopped.trySuccess(Stopped(keyedEventJournalingActorCount = journalingActors.size))
    for (a ← journalingActors) logger.debug(s"Journal stopped while a JournalingActor is still running: ${a.path}")
    if (temporaryJournalWriter != null) {
      logger.debug(s"Deleting temporary journal files due to termination: ${temporaryJournalWriter.file}")
      temporaryJournalWriter.close()
      Files.delete(temporaryJournalWriter.file)
    }
    if (journalWriter != null) {
      journalWriter.close()
      journalWriter.logStatistics()
    }
    logger.debug("Stopped")
    super.postStop()
  }

  def receive = {
    case Input.Start(RecoveredJournalingActors(keyToActor), eventWatchOption_, lastEventId) ⇒
      lastWrittenEventId = lastEventId
      eventWatchOption = eventWatchOption_ map (_.asInstanceOf[JournalEventWatch[E]]/*restore erased type argument, not checked*/)
      eventIdGenerator.updateLastEventId(lastEventId)
      journalingActors ++= keyToActor.values
      journalingActors foreach watch
      val sender = this.sender()
      becomeTakingSnapshotThen() {
        unstashAll()
        becomeReady()
        sender ! Output.Ready
      }

    case Input.StartWithoutRecovery ⇒  // Testing only
      journalWriter = newJsonWriter(journalMeta.file(after = lastWrittenEventId))
      unstashAll()
      journalWriter.beginSnapshotSection()  // Empty snapshot section
      becomeReady()
      sender() ! Output.Ready

    case Input.RegisterMe ⇒
      handleRegisterMe()

    case _ ⇒
      stash()
  }

  private def becomeReady(): Unit = {
    become(ready)
    logger.info(s"Ready, writing ${if (syncOnCommit) "(with sync)" else "(without sync)"} journal file '${journalWriter.file}'")
    journalWriter.beginEventSection()
  }

  private def ready: Receive = receiveTerminatedOrGet orElse {
    case Input.EventsAccepted(untilEventId) ⇒
      if (untilEventId > lastWrittenEventId)
        sender() ! Invalid(Problem(s"EventsAccepted($untilEventId): unknown EventId"))
      else {
        for (r ← eventWatchOption) r.onEventsAcceptedUntil(untilEventId)
        sender() ! Valid(Completed)
      }

    case Input.RegisterMe ⇒
      handleRegisterMe()

    case Input.Store(keyedEvents, replyTo, timestampOption, noSync, item) ⇒
      val stampedOptions = keyedEvents map { _ map { e ⇒ eventIdGenerator.stamp(e.asInstanceOf[KeyedEvent[E]], timestampOption) }}
      try journalWriter.writeEvents(stampedOptions.flatten)
      catch {
        case t: JournalWriter.SerializationException ⇒
          logger.error(t.getCause.toStringWithCauses, t.getCause)
          replyTo.forward(Output.SerializationFailure(t.getCause))  // TODO Handle message in JournaledActor
        case NonFatal(t) ⇒
          val tt = t.appendCurrentStackTrace
          logger.error(tt.toStringWithCauses, tt)
          replyTo.forward(Output.StoreFailure(tt))  // TODO Handle message in JournaledActor
          throw tt  // Stop Actor
      }
      writtenBuffer += Written(stampedOptions, replyTo, sender(), item)
      dontSync &= noSync
      forwardCommit()

    case Internal.Commit(level) ⇒
      if (level < writtenBuffer.length)
        // writtenBuffer has grown? Queue again to coalesce two commits
        forwardCommit()
      else
      if (level == writtenBuffer.length) {  // writtenBuffer has not grown since last issued Commit
        val sync = syncOnCommit && !dontSync
        try {
          journalWriter.flush()
          if (sync) journalWriter.sync()
        } catch { case NonFatal(t) ⇒
          val tt = t.appendCurrentStackTrace
          for (w ← writtenBuffer) w.replyTo.!(Output.StoreFailure(tt))(sender)
          throw tt;
        }
        logWrittenAsStored(sync)
        for (Written(stampedOptions, replyTo, sender, item) ← writtenBuffer) {
          replyTo.!(Output.Stored(stampedOptions, item))(sender)
          for (lastStamped ← stampedOptions.reverseIterator.flatten.buffered.headOption) {
            lastWrittenEventId = lastStamped.eventId
          }
          for (stamped ← stampedOptions.iterator.flatten) {
            keyedEventBus.publish(stamped)
          }
        }
        writtenBuffer.clear()
        dontSync = true
      } else
      if (writtenBuffer.nonEmpty) {
        logger.trace(s"Discarded: Commit($level), writtenBuffer.length=${writtenBuffer.length}")
      }

    case Input.TakeSnapshot ⇒
      if (!journalWriter.isEventWritten) {
        sender ! Output.SnapshotTaken
      } else {
        journalWriter.close()
        journalWriter.logStatistics()
        val sender = this.sender()
        becomeTakingSnapshotThen() {
          becomeReady()  // Writes EventHeader
          sender ! Output.SnapshotTaken
        }
      }

    case Input.Terminate ⇒
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

  private def receiveTerminatedOrGet: Receive = {
    case Terminated(a) if journalingActors contains a ⇒
      logger.trace(s"Terminated: ${a.path}")
      journalingActors -= a

    case Input.GetState ⇒
      sender() ! (
        if (journalWriter == null)
          Output.State(isFlushed = false, isSynced = false)
        else
          Output.State(isFlushed = journalWriter.isFlushed, isSynced = journalWriter.isSynced))
  }

  private def logWrittenAsStored(synced: Boolean) =
    if (logger.underlying.isTraceEnabled) {
      val it = writtenBuffer.iterator.flatMap(_.stampeds).flatten
      while (it.hasNext) {
        val stamped = it.next()
        val last = if (it.hasNext) "     " else if (synced) "sync " else "flush"  // After the last one, the file buffer was flushed
        logger.trace(s"$last STORED ${stamped.eventId} ${stamped.value}")
      }
    }

  private def becomeTakingSnapshotThen()(andThen: ⇒ Unit) = {
    if (journalWriter != null) {
      journalWriter.close()
    }
    temporaryJournalWriter = newJsonWriter(journalMeta.file(after = lastWrittenEventId, extraSuffix = ".tmp"))
    temporaryJournalWriter.beginSnapshotSection()
    actorOf(
      Props { new SnapshotWriter(temporaryJournalWriter.writeSnapshot, journalingActors.toSet, snapshotJsonCodec) },
      uniqueActorName("SnapshotWriter"))
    become(takingSnapshot(commander = sender(), () ⇒ andThen, new Stopwatch))
  }

  private def takingSnapshot(commander: ActorRef, andThen: () ⇒ Unit, stopwatch: Stopwatch): Receive = {
    case SnapshotWriter.Output.Finished(Failure(t)) ⇒
      throw t.appendCurrentStackTrace

    case SnapshotWriter.Output.Finished(Success(snapshotCount)) ⇒
      temporaryJournalWriter.close()
      if (stopwatch.duration >= 1.s) logger.debug(stopwatch.itemsPerSecondString(snapshotCount, "snapshots") + " written")
      if (snapshotCount > 0) {
        logger.info(s"$snapshotCount snapshots written to journal")
      }
      val file = journalMeta.file(after = lastWrittenEventId)
      move(temporaryJournalWriter.file, file, ATOMIC_MOVE)
      temporaryJournalWriter = null
      journalWriter = newJsonWriter(file, appendToSnapshots = true)
      unstashAll()
      andThen()

    case _ ⇒
      stash()
  }

  private def newJsonWriter(file: Path, appendToSnapshots: Boolean = false) =
    new JournalWriter[E](journalMeta, file, after = lastWrittenEventId, eventWatchOption, appendToSnapshots = appendToSnapshots,
      simulateSync = simulateSync)

  private def handleRegisterMe() = {
    journalingActors += sender()
    watch(sender())
  }
}

object JournalActor
{
  def props[E <: Event](
    journalMeta: JournalMeta[E],
    config: Config,
    keyedEventBus: StampedKeyedEventBus,
    scheduler: Scheduler,
    stopped: Promise[Stopped] = Promise(),
    eventIdGenerator: EventIdGenerator = new EventIdGenerator)
  =
    Props { new JournalActor(journalMeta, config, keyedEventBus, scheduler, stopped, eventIdGenerator) }

  trait CallersItem

  object Input {
    private[journal] final case class Start(
      recoveredJournalingActors: RecoveredJournalingActors,
      eventWatch: Option[JournalEventWatch[_ <: Event]],
      lastEventId: EventId)
    final case object StartWithoutRecovery
    final case class EventsAccepted(untilEventId: EventId)
    private[journal] case object RegisterMe
    private[journal] final case class Store(
      keyedEventOptions: Seq[Option[AnyKeyedEvent]],
      journalingActor: ActorRef,
      timestamp: Option[Timestamp],
      noSync: Boolean,
      item: CallersItem)
    final case object TakeSnapshot
    final case object Terminate
    final case object AwaitAndTerminate
    private[journal] case object GetState
  }

  sealed trait Output
  object Output {
    final case object Ready
    private[journal] final case class Stored(stamped: Seq[Option[Stamped[AnyKeyedEvent]]], item: CallersItem) extends Output
    final case class SerializationFailure(throwable: Throwable) extends Output
    final case class StoreFailure(throwable: Throwable) extends Output
    final case object SnapshotTaken
    private[journal] final case class State(isFlushed: Boolean, isSynced: Boolean)
  }

  final case class Stopped(keyedEventJournalingActorCount: Int)

  private object Internal {
    final case class Commit(writtenLevel: Int)
  }

  private case class Written(
    stampeds: Seq[Option[Stamped[AnyKeyedEvent]]],  // None means no-operation (for deferAsync)
    replyTo: ActorRef,
    sender: ActorRef,
    item: CallersItem)
  {
    def lastStamped: Option[Stamped[AnyKeyedEvent]] =
      stampeds.reverseIterator.flatten.buffered.headOption
  }
}

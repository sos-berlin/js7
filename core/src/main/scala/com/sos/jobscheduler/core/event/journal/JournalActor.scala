package com.sos.jobscheduler.core.event.journal

import akka.actor._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.akkautils.Akkas.uniqueActorName
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.JournalActor._
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import java.nio.file.Files.move
import java.nio.file.StandardCopyOption.{ATOMIC_MOVE, REPLACE_EXISTING}
import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Promise
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class JournalActor[E <: Event] private(
  meta: JournalMeta[E],
  file: Path,
  syncOnCommit: Boolean,
  keyedEventBus: StampedKeyedEventBus,
  stopped: Promise[Stopped],
  eventIdGenerator: EventIdGenerator)
extends Actor with Stash {

  import meta.snapshotJsonCodec

  private val logger = Logger.withPrefix[JournalActor[_]](file.getFileName.toString)
  override val supervisorStrategy = SupervisorStrategies.escalate

  private var journalWriter: JournalWriter[E] = null
  private var eventReaderOptionProvider: Option[JournalEventReaderProvider[E]] = None
  private var temporaryJournalWriter: JournalWriter[E] = null
  private val keyToJournalingActor = mutable.Map[Any, ActorRef]()
  private val keylessJournalingActors = mutable.Set[ActorRef]()
  private val writtenBuffer = mutable.ArrayBuffer[Written]()       // TODO Avoid OutOfMemoryError and commit when written JSON becomes big
  private var dontSync = true

  override def postStop() = {
    stopped.trySuccess(Stopped(keyedEventJournalingActorCount = keyToJournalingActor.size))
    for (key ← keyToJournalingActor.keys) logger.debug(s"Journal stopped but a KeyedJournalingActor is still running for key=$key")
    for (a ← keylessJournalingActors) logger.debug(s"Journal stopped but a JournalingActor is still running for $a")
    if (temporaryJournalWriter != null) {
      logger.debug(s"Deleting temporary journal file due to termination: ${temporaryJournalWriter.file}")
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
    case Input.Start(RecoveredJournalingActors(keyToActor), eventReaderProviderOption_, eventsAcceptedUntil, lastEventId) ⇒
      eventIdGenerator.updateLastEventId(lastEventId)
      keyToJournalingActor ++= keyToActor
      keyToJournalingActor.values foreach context.watch
      eventReaderOptionProvider = eventReaderProviderOption_ map (_.asInstanceOf[JournalEventReaderProvider[E]]/*restore erased type argument, not checked*/)
      val sender = this.sender()
      becomeTakingSnapshotThen(lastEventId = lastEventId) {
        unstashAll()
        becomeReady()
        sender ! Output.Ready
      }

    case Input.StartWithoutRecovery ⇒
      journalWriter = newJsonWriter(file, appendToSnapshots = false)
      unstashAll()
      becomeReady()
      sender() ! Output.Ready

    case msg: Input.RegisterMe ⇒
      handleRegisterMe(msg)

    case _ ⇒
      stash()
  }

  private def becomeReady(): Unit = {
    context.become(ready)
    logger.info(s"Ready, writing ${if (syncOnCommit) "(with sync)" else "(without sync)"} journal file '${journalWriter.file}'")
    journalWriter.startEvents()
  }

  private def ready: Receive = {
    case msg: Input.RegisterMe ⇒
      handleRegisterMe(msg)

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
      self.forward(Internal.Commit(writtenBuffer.length))  // Commit after possibly outstanding Input.Store messages

    case Internal.Commit(level) ⇒
      if (level < writtenBuffer.length) {
        self.forward(Internal.Commit(writtenBuffer.length))  // writtenBuffer has grown? Queue again to coalesce two commits
      } else
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
          for (stampedOption ← stampedOptions; stamped ← stampedOption) {
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
        becomeTakingSnapshotThen(lastEventId = journalWriter.lastEventId) {
          becomeReady()  // Writes EventHeader
          sender ! Output.SnapshotTaken
        }
      }

    case Input.Terminate ⇒
      context.stop(self)

    case Input.GetState ⇒
      sender() ! (
        if (journalWriter == null)
          Output.State(isFlushed = false, isSynced = false)
        else
          Output.State(isFlushed = journalWriter.isFlushed, isSynced = journalWriter.isSynced))

    case Terminated(a) ⇒
      val keys = keyToJournalingActor collect { case (k, `a`) ⇒ k }
      logger.trace(s"Terminated: ${keys mkString "?"} -> ${a.path}")
      keyToJournalingActor --= keys
      keylessJournalingActors -= a
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

  private def becomeTakingSnapshotThen(lastEventId: EventId)(andThen: ⇒ Unit) = {
    logger.debug(s"Taking snapshot lastEventId=${EventId.toString(lastEventId)}")
    if (journalWriter != null) {
      journalWriter.close()
    }
    temporaryJournalWriter = newJsonWriter(Paths.get(s"$file.tmp"), appendToSnapshots = false)
    temporaryJournalWriter.startSnapshots(lastEventId = lastEventId)
    val journalingActors = keyToJournalingActor.values.toSet ++ keylessJournalingActors
    context.actorOf(
      Props { new SnapshotWriter(temporaryJournalWriter.writeSnapshot, journalingActors, snapshotJsonCodec) },
      uniqueActorName("SnapshotWriter"))
    context.become(takingSnapshot(commander = sender(), () ⇒ andThen, new Stopwatch))
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
      if (journalWriter != null) {
        journalWriter.close()
      }
      move(temporaryJournalWriter.file, file, REPLACE_EXISTING, ATOMIC_MOVE)   // TODO Gibt es für kurze Zeit nur das journal.tmp ?
      temporaryJournalWriter = null

      journalWriter = newJsonWriter(file, appendToSnapshots = true)
      unstashAll()
      andThen()

    case _ ⇒
      stash()
  }

  private def newJsonWriter(file: Path, appendToSnapshots: Boolean) =
    new JournalWriter[E](meta, file, appendToSnapshots = appendToSnapshots, eventReaderOptionProvider)

  private def handleRegisterMe(msg: Input.RegisterMe) = msg match {
    case Input.RegisterMe(None) ⇒
      keylessJournalingActors.add(sender())
      context.watch(sender())

    case Input.RegisterMe(Some(key)) ⇒
      keyToJournalingActor += key → sender()
      context.watch(sender())
  }
}

object JournalActor
{
  def props[E <: Event](
    meta: JournalMeta[E],
    file: Path,
    syncOnCommit: Boolean,
    keyedEventBus: StampedKeyedEventBus,
    stopped: Promise[Stopped] = Promise(),
    eventIdGenerator: EventIdGenerator = new EventIdGenerator)
  =
    Props { new JournalActor(meta, file, syncOnCommit, keyedEventBus, stopped, eventIdGenerator) }

  trait CallersItem

  object Input {
    private[journal] final case class Start(
      recoveredJournalingActors: RecoveredJournalingActors,
      eventReader: Option[JournalEventReaderProvider[_ <: Event]],
      eventsAcceptedUntil: EventId,
      lastEventId: EventId)
    final case object StartWithoutRecovery
    final case class RegisterMe(key: Option[Any])
    final case class Store(
      keyedEventOptions: Seq[Option[AnyKeyedEvent]],
      journalingActor: ActorRef,
      timestamp: Option[Timestamp],
      noSync: Boolean,
      item: CallersItem)
    final case object TakeSnapshot
    final case object Terminate
    private[journal] case object GetState
  }

  sealed trait Output
  object Output {
    final case object Ready
    final case class Stored(stamped: Seq[Option[Stamped[AnyKeyedEvent]]], item: CallersItem) extends Output
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
}

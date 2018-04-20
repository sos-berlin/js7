package com.sos.jobscheduler.core.event.journal

import akka.actor._
import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.akkautils.Akkas.uniqueActorName
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.utils.ByteUnits.toMB
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.JournalActor._
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, KeyedEvent, Stamped}
import io.circe.Json
import io.circe.syntax.EncoderOps
import java.nio.file.Files.move
import java.nio.file.StandardCopyOption.{ATOMIC_MOVE, REPLACE_EXISTING}
import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Promise
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class JournalActor[E <: Event] private(
  meta: JournalMeta[E],
  file: Path,
  syncOnCommit: Boolean,
  eventIdGenerator: EventIdGenerator,
  keyedEventBus: StampedKeyedEventBus,
  stopped: Promise[Stopped])
extends Actor with Stash {

  import meta.{convertOutputStream, eventJsonCodec, snapshotJsonCodec}

  private val logger = Logger.withPrefix[JournalActor[_]](file.getFileName.toString)
  override val supervisorStrategy = SupervisorStrategies.escalate

  private var jsonWriter: FileJsonWriter = null
  private var temporaryJsonWriter: FileJsonWriter = null
  private val keyToJournalingActor = mutable.Map[Any, ActorRef]()
  private val keylessJournalingActors = mutable.Set[ActorRef]()
  private val writtenBuffer = mutable.ArrayBuffer[Written]()       // TODO Avoid OutOfMemoryError and commit when written JSON becomes big
  private var dontSync = true
  private val statistics = new StatisticsCounter

  override def postStop() = {
    stopped.success(Stopped(keyedEventJournalingActorCount = keyToJournalingActor.size))
    for (key ← keyToJournalingActor.keys) logger.debug(s"Journal stopped but a KeyedJournalingActor is still running for key=$key")
    for (a ← keylessJournalingActors) logger.debug(s"Journal stopped but a JournalingActor is still running for $a")
    if (temporaryJsonWriter != null) {
      logger.debug(s"Deleting temporary journal file due to termination: ${temporaryJsonWriter.file}")
      temporaryJsonWriter.close()
      Files.delete(temporaryJsonWriter.file)
    }
    val msg = if (jsonWriter != null) {
      jsonWriter.close()
      val s = try toMB(Files.size(file)) catch { case NonFatal(t) ⇒ t.toString }
      s" $s written. $statistics"
    } else ""
    logger.info(s"Stopped.$msg")
    logger.debug(statistics.timingString)
    super.postStop()
  }

  def receive = {
    case Input.Start(RecoveredJournalingActors(keyToActor)) ⇒
      keyToJournalingActor ++= keyToActor
      keyToJournalingActor.values foreach context.watch
      val sender = this.sender()
      becomeTakingSnapshotThen {
        unstashAll()
        becomeReady()
        sender ! Output.Ready
      }

    case Input.StartWithoutRecovery ⇒
      jsonWriter = newJsonWriter(file, append = false)
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
    logger.info(s"Ready, writing ${if (syncOnCommit) "(with sync)" else "(without sync)"} journal file '${jsonWriter.file}'")
    jsonWriter.writeJson(ByteString(EventsHeader.compactPrint))
  }

  private def ready: Receive = {
    case msg: Input.RegisterMe ⇒
      handleRegisterMe(msg)

    case Input.Store(keyedEvents, replyTo, timestampOption, noSync, item) ⇒
      val stampedOptions = keyedEvents map { _ map { e ⇒ eventIdGenerator.stamp(e.asInstanceOf[KeyedEvent[E]], timestampOption) }}
      Try {
        stampedOptions.flatten map { _.asJson }
      } match {
        case Success(jsons) ⇒
          writeToDisk(jsons, replyTo)
          writtenBuffer += Written(stampedOptions, replyTo, sender(), item)
          dontSync &= noSync
          statistics.countWillBeCommittedEvents(jsons.size) // ???
          self.forward(Internal.Commit(writtenBuffer.length))  // Commit after possibly outstanding Input.Store messages

        case Failure(t) ⇒
          logger.error(s"$t", t)
          replyTo.forward(Output.SerializationFailure(t))  // TODO Handle message in JournaledActor
      }

    case Internal.Commit(level) ⇒
      if (level < writtenBuffer.length) {
        self.forward(Internal.Commit(writtenBuffer.length))  // writtenBuffer has grown? Queue again to coalesce two commits
      } else
      if (level == writtenBuffer.length) {  // writtenBuffer has not grown since last issued Commit
        val sync = syncOnCommit && !dontSync
        try flush(sync = sync)
        catch { case NonFatal(t) ⇒
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
      jsonWriter.close()
      val sender = this.sender()
      becomeTakingSnapshotThen {
        sender ! Output.SnapshotTaken
        becomeReady()
      }

    case Input.Terminate ⇒
      context.stop(self)

    case Input.GetState ⇒
      sender() ! (
        if (jsonWriter == null)
          Output.State(isFlushed = false, isSynced = false)
        else
          Output.State(isFlushed = jsonWriter.isFlushed, isSynced = jsonWriter.isSynced))

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

  private def becomeTakingSnapshotThen(andThen: ⇒ Unit) = {
    logger.debug(s"Taking snapshot")
    if (jsonWriter != null) {
      jsonWriter.close()
    }
    temporaryJsonWriter = newJsonWriter(Paths.get(s"$file.tmp"), append = false)
    temporaryJsonWriter.writeJson(ByteString(SnapshotsHeader.compactPrint))
    val journalingActors = keyToJournalingActor.values.toSet ++ keylessJournalingActors
    context.actorOf(
      Props { new SnapshotWriter(temporaryJsonWriter.writeJson, journalingActors, snapshotJsonCodec) },
      uniqueActorName("SnapshotWriter"))
    context.become(takingSnapshot(commander = sender(), () ⇒ andThen, new Stopwatch))
  }

  private def takingSnapshot(commander: ActorRef, andThen: () ⇒ Unit, stopwatch: Stopwatch): Receive = {
    case SnapshotWriter.Output.Finished(done) ⇒
      temporaryJsonWriter.close()
      val snapshotCount = done.get  // Crash !!!
      if (stopwatch.duration >= 1.s) logger.debug(stopwatch.itemsPerSecondString(snapshotCount, "snapshots") + " written")
      if (snapshotCount > 0) {
        logger.info(s"$snapshotCount snapshots written to journal")
      }

      if (jsonWriter != null) {
        jsonWriter.close()
      }
      move(temporaryJsonWriter.file, file, REPLACE_EXISTING, ATOMIC_MOVE)   // TODO Gibt es für kurze Zeit nur das journal.tmp ?
      temporaryJsonWriter = null

      jsonWriter = newJsonWriter(file, append = true)
      unstashAll()
      andThen()

    case _ ⇒
      stash()
  }

  private def newJsonWriter(file: Path, append: Boolean) =
    new FileJsonWriter(JournalMeta.header, convertOutputStream, file, append = append)

  private def writeToDisk(jsons: Seq[Json], errorReplyTo: ActorRef): Unit =
    try for (jsValue ← jsons) jsonWriter.writeJson(ByteString(jsValue.compactPrint))
    catch { case NonFatal(t) ⇒
      val tt = t.appendCurrentStackTrace
      logger.error(s"$t", t)
      errorReplyTo.forward(Output.StoreFailure(tt))  // TODO Handle message in JournaledActor
      throw tt  // Stop Actor
    }

  private def flush(sync: Boolean): Unit = {
    statistics.beforeFlush()
    jsonWriter.flush()
    statistics.afterFlush()
    if (sync) {
      statistics.beforeSync()
      jsonWriter.sync()
      statistics.afterSync()
    }
  }

  private def handleRegisterMe(msg: Input.RegisterMe) = msg match {
    case Input.RegisterMe(None) ⇒
      keylessJournalingActors.add(sender())
      context.watch(sender())

    case Input.RegisterMe(Some(key)) ⇒
      keyToJournalingActor += key → sender()
      context.watch(sender())
  }
}

object JournalActor {
  trait CallersItem
  val SnapshotsHeader = Json.fromString("SNAPSHOTS")
  val EventsHeader = Json.fromString("EVENTS")

  def props[E <: Event](meta: JournalMeta[E], file: Path, syncOnCommit: Boolean,
    eventIdGenerator: EventIdGenerator, keyedEventBus: StampedKeyedEventBus, stopped: Promise[Stopped] = Promise())
  =
    Props { new JournalActor(meta, file, syncOnCommit, eventIdGenerator, keyedEventBus, stopped) }

  object Input {
    private[journal] final case class Start(recoveredJournalingActors: RecoveredJournalingActors)
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

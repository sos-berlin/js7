package com.sos.jobscheduler.shared.event.journal

import akka.actor._
import akka.util.ByteString
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, KeyedEvent, Stamped}
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.JsonJournalActor._
import com.sos.jobscheduler.shared.event.journal.JsonJournalMeta.Header
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer.{RecoveredJournalingActors, toMB}
import java.nio.file.Files.move
import java.nio.file.StandardCopyOption.{ATOMIC_MOVE, REPLACE_EXISTING}
import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class JsonJournalActor[E <: Event](
  meta: JsonJournalMeta[E],
  file: Path,
  syncOnCommit: Boolean,
  eventIdGenerator: EventIdGenerator,
  keyedEventBus: StampedKeyedEventBus)
extends Actor with Stash {

  import meta.{convertOutputStream, eventJsonFormat, snapshotJsonFormat}

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 0) {
    case _ ⇒ SupervisorStrategy.Stop
  }

  private var jsonWriter: FileJsonWriter = null
  private val keyToJournalingActor = mutable.Map[Any, ActorRef]()
  private val keylessJournalingActors = mutable.Set[ActorRef]()
  private val writtenBuffer = mutable.ArrayBuffer[Written]()
  private object statistics {
    var commits = 0
    var flushes = 0
    override def toString =
      if (jsonWriter == null || flushes == 0) ""
      else {
        val ops = if (jsonWriter.syncOnFlush) "syncs" else "flushs"
        f"$commits commits ($flushes $ops)" //, coalescence factor ${commits.toDouble / flushes}%1.1f)"
      }
  }

  private val logger = Logger.withPrefix[JsonJournalActor[_]](file.getFileName.toString)

  override def postStop() = {
    val msg = if (jsonWriter != null) {
      jsonWriter.close()
      val s = try toMB(Files.size(file)) catch { case NonFatal(t) ⇒ t.toString }
      s" $s written. $statistics"
    } else ""
    logger.info(s"Stopped.$msg")
    super.postStop()
  }

  def receive = {
    case Input.Start(RecoveredJournalingActors(keyToActor)) ⇒
      keyToJournalingActor ++= keyToActor
      val sender = this.sender()
      becomeTakingSnapshotThen {
        unstashAll()
        becomeReady(sender)
      }

    case Input.StartWithoutRecovery ⇒
      jsonWriter = newJsonWriter(file, append = false)
      unstashAll()
      becomeReady(sender())

    case msg: Input.RegisterMe ⇒
      handleRegisterMe(msg)

    case _ ⇒
      stash()
  }

  private def becomeReady(sender: ActorRef): Unit = {
    context.become(ready)
    logger.info(s"Ready, writing ${if (jsonWriter.syncOnFlush) "(with sync)" else "(without sync)"} journal file '${jsonWriter.file}'")
    sender ! Output.Ready
  }

  private def ready: Receive = {
    case msg: Input.RegisterMe ⇒
      handleRegisterMe(msg)

    case Input.Store(keyedEvents, replyTo) ⇒
      val stampedOptions = keyedEvents map { _ map { e ⇒ eventIdGenerator.stamp(e.asInstanceOf[KeyedEvent[E]]) }}
      Try {
        stampedOptions.flatten map { _.toJson }
      } match {
        case Success(jsValueOptions) ⇒
          writeToDisk(jsValueOptions, replyTo)
          writtenBuffer += Written(stampedOptions, replyTo, sender())
          self.forward(Internal.Commit(writtenBuffer.length))  // Commit after possibly outstanding Input.Store messages
          statistics.commits += 1

        case Failure(t) ⇒
          logger.error(s"$t")
          replyTo.forward(Output.SerializationFailure(t))  // TODO Handle message in JournaledActor
      }

    case Internal.Commit(level) ⇒
      if (level < writtenBuffer.length) {
        self.forward(Internal.Commit(writtenBuffer.length))  // storedBuffer has grown? Queue again to coalesce two commits
      } else
      if (level == writtenBuffer.length) {  // storedBuffer has not grown since last issued Commit
        try jsonWriter.flush()
        catch {
          case NonFatal(t) ⇒
            val tt = t.appendCurrentStackTrace
            for (w ← writtenBuffer) w.replyTo.!(Output.StoreFailure(tt))(sender)
            throw tt;
        }
        statistics.flushes += 1
        logger.trace(s"${if (jsonWriter.syncOnFlush) "Synced" else "Flushed"} ${(writtenBuffer map { _.eventSnapshots.size }).sum} events")
        for (Written(stampedOptions, replyTo, sender) ← writtenBuffer) {
          replyTo.!(Output.Stored(stampedOptions))(sender)
          for (stampedOption ← stampedOptions; stamped ← stampedOption) {
            logger.trace(s"STORED $stamped")
            keyedEventBus.publish(stamped)
          }
        }
        writtenBuffer.clear()
      } else
      if (writtenBuffer.nonEmpty) {
        logger.trace(s"Discarded: Commit($level), writtenBuffer.length=${writtenBuffer.length}")
      }

    case Input.TakeSnapshot ⇒
      jsonWriter.close()
      val sender = this.sender()
      becomeTakingSnapshotThen {
        sender ! Output.SnapshotTaken
        context.become(ready)
      }

    case Terminated(a) ⇒
      val keys = keyToJournalingActor collect { case (k, `a`) ⇒ k }
      logger.trace(s"Unregistering $keys -> ${a.path}")
      keyToJournalingActor --= keys
      keylessJournalingActors -= a
  }

  private def becomeTakingSnapshotThen(andThen: ⇒ Unit) = {
    logger.debug(s"Taking snapshot")
    if (jsonWriter != null) {
      jsonWriter.close()
    }
    val myJsonWriter = newJsonWriter(Paths.get(s"$file.tmp"), append = false)
    val journalingActors = keyToJournalingActor.values.toSet ++ keylessJournalingActors
    context.actorOf(
      Props { new SnapshotWriter(myJsonWriter.writeJson, journalingActors, snapshotJsonFormat) },
      "SnapshotWriter")
    context.become(takingSnapshot(myJsonWriter, commander = sender(), () ⇒ andThen, new Stopwatch))
  }

  private def takingSnapshot(myJsonWriter: FileJsonWriter, commander: ActorRef, andThen: () ⇒ Unit, stopwatch: Stopwatch): Receive = {
    case SnapshotWriter.Output.Finished(done) ⇒
      myJsonWriter.close()
      val snapshotCount = done.get  // Crash !!!
      if (stopwatch.duration >= 1.s) logger.debug(stopwatch.itemsPerSecondString(snapshotCount, "objects"))
      if (snapshotCount > 0) {
        logger.info(s"$snapshotCount snapshot written to journal")
      }

      if (jsonWriter != null) {
        jsonWriter.close()
      }
      move(myJsonWriter.file, file, REPLACE_EXISTING, ATOMIC_MOVE)   // TODO Gibt es für kurze Zeit nur das journal.tmp ?

      jsonWriter = newJsonWriter(file, append = true)
      unstashAll()
      andThen()

    case _ ⇒
      stash()
  }

  private def newJsonWriter(file: Path, append: Boolean) =
    new FileJsonWriter(Header, convertOutputStream, file, syncOnFlush = syncOnCommit, append = append)

  private def writeToDisk(jsValues: Seq[JsValue], errorReplyTo: ActorRef): Unit =
    try {
      for (jsValue ← jsValues) jsonWriter.writeJson(ByteString(CompactPrinter(jsValue)))
    } catch { case NonFatal(t) ⇒
      val tt = t.appendCurrentStackTrace
      logger.error(s"$t")
      errorReplyTo.forward(Output.StoreFailure(tt))  // TODO Handle message in JournaledActor
      throw tt  // Stop Actor
    }

  private def handleRegisterMe(msg: Input.RegisterMe) = msg match {
    case Input.RegisterMe(None) ⇒
      keylessJournalingActors += sender()

    case Input.RegisterMe(Some(key)) ⇒
      keyToJournalingActor += key → sender()
  }
}

object JsonJournalActor {

  object Input {
    private[journal] final case class Start(recoveredJournalingActors: RecoveredJournalingActors)
    final case object StartWithoutRecovery
    final case class RegisterMe(key: Option[Any])
    final case class Store(eventStampeds: Seq[Option[AnyKeyedEvent]], journalingActor: ActorRef)
    final case object TakeSnapshot
  }

  sealed trait Output
  object Output {
    final case object Ready
    final case class Stored(stamped: Seq[Option[Stamped[AnyKeyedEvent]]]) extends Output
    final case class SerializationFailure(throwable: Throwable) extends Output
    final case class StoreFailure(throwable: Throwable) extends Output
    final case object SnapshotTaken
  }

  private object Internal {
    final case class Commit(writtenLevel: Int)
  }

  private case class Written(
    eventSnapshots: Seq[Option[Stamped[AnyKeyedEvent]]],  // None means no-operation (for deferAsync)
    replyTo: ActorRef, sender: ActorRef)
}

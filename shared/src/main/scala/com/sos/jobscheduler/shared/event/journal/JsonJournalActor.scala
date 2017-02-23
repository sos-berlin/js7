package com.sos.jobscheduler.shared.event.journal

import akka.actor._
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Snapshot}
import com.sos.jobscheduler.shared.event.SnapshotKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.Journal.{Input, Output}
import com.sos.jobscheduler.shared.event.journal.JsonJournalActor._
import com.sos.jobscheduler.shared.event.journal.JsonJournalMeta.Header
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer.RecoveredJournalingActors
import java.nio.file.Files.move
import java.nio.file.StandardCopyOption.{ATOMIC_MOVE, REPLACE_EXISTING}
import java.nio.file.{Path, Paths}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class JsonJournalActor(
  meta: JsonJournalMeta,
  file: Path,
  syncOnCommit: Boolean,
  eventIdGenerator: EventIdGenerator,
  keyedEventBus: SnapshotKeyedEventBus)
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
        f"$flushes $ops for $commits commits (coalescence factor ${commits.toDouble / flushes}%1.1f)"
      }
  }

  override def postStop() = {
    if (jsonWriter != null) jsonWriter.close()
    logger.debug(s"Stopped. $statistics".trim)
    super.postStop()
  }

  def receive = {
    case Input.Start(RecoveredJournalingActors(keyToActor)) ⇒
      keyToJournalingActor ++= keyToActor
      val sender = this.sender()
      becomeTakingSnapshot {
        context.become(ready)
        sender ! Output.Ready
        logger.info(s"Ready, writing ${if (jsonWriter.syncOnFlush) "(with sync)" else "(without sync)"} journal file '${jsonWriter.file}'")
      }

    case msg: Input.RegisterMe ⇒
      handleRegisterMe(msg)

    case _ ⇒
      stash()
  }

  private def ready: Receive = {
    case msg: Input.RegisterMe ⇒
      handleRegisterMe(msg)

    case Input.Store(keyedEvents, replyTo) ⇒
      val eventSnapshots = keyedEvents map eventIdGenerator.newSnapshot
      Try {
        eventSnapshots map { _.toJson }
      } match {
        case Success(jsValues) ⇒
          writeToDisk(jsValues, replyTo)
          writtenBuffer += Written(eventSnapshots, replyTo, sender())
          self.forward(Internal.Commit(writtenBuffer.length))  // Sync after possibly outstanding Input.Store messages
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
          case t: Throwable ⇒ for (w ← writtenBuffer) w.replyTo.tell(Output.StoreFailure(t), sender)
        }
        statistics.flushes += 1
        logger.trace(s"${if (jsonWriter.syncOnFlush) "Synced" else "Flushed"} ${(writtenBuffer map { _.eventSnapshots.size }).sum} events")
        for (Written(eventSnapshots, replyTo, sender) ← writtenBuffer) {
          replyTo.tell(Output.Stored(eventSnapshots), sender)
          for (eventSnapshot ← eventSnapshots) {
            logger.trace(s"STORED $eventSnapshot")
            keyedEventBus.publish(eventSnapshot)
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
      becomeTakingSnapshot {
        sender ! Output.SnapshotTaken
        context.become(ready)
      }

    case Terminated(a) ⇒
      val keys = keyToJournalingActor collect { case (k, `a`) ⇒ k }
      logger.trace(s"Unregistering $keys -> ${a.path}")
      keyToJournalingActor --= keys
      keylessJournalingActors -= a
  }

  private def becomeTakingSnapshot(andThen: ⇒ Unit) = {
    logger.info(s"Taking snapshot")
    if (jsonWriter != null) {
      jsonWriter.close()
    }
    val myJsonWriter = newJsonWriter(Paths.get(s"$file~"), append = false)
    val snapshotWriter = context.actorOf(
      Props { new SnapshotWriter(myJsonWriter.writeJson, keyToJournalingActor.values.toSet ++ keylessJournalingActors, snapshotJsonFormat) },
      "SnapshotWriter")
    context.become(takingSnapshot(myJsonWriter, snapshotWriter, commander = sender(), andThen))
  }

  private def handleRegisterMe(msg: Input.RegisterMe) = msg match {
    case Input.RegisterMe(None) ⇒
      keylessJournalingActors += sender()

    case Input.RegisterMe(Some(key)) ⇒
      keyToJournalingActor += key → sender()
  }

  private def takingSnapshot(myJsonWriter: FileJsonWriter, from: ActorRef, commander: ActorRef, andThen: ⇒ Unit): Receive = {
    case SnapshotWriter.Output.Finished(done) if sender() == from ⇒
      myJsonWriter.close()
      val snapshotCount = done.get  // Crash !!!
      logger.info(s"Snapshot contains $snapshotCount objects")

      if (jsonWriter != null) {
        jsonWriter.close()
      }
      move(myJsonWriter.file, file, REPLACE_EXISTING, ATOMIC_MOVE)   // TODO Gibt es für kurze Zeit nur das journal~ ?

      jsonWriter = newJsonWriter(file, append = true)
      unstashAll()
      andThen

    case _ ⇒
      stash()
  }

  private def newJsonWriter(file: Path, append: Boolean) =
    new FileJsonWriter(Header, convertOutputStream, file, syncOnFlush = syncOnCommit, append = append)

  private def writeToDisk(jsValues: Seq[JsValue], errorReplyTo: ActorRef): Unit =
    try jsValues foreach jsonWriter.writeJson
    catch {
      case NonFatal(t) ⇒
        logger.error(s"$t")
        errorReplyTo.forward(Output.StoreFailure(t))  // TODO Handle message in JournaledActor
        throw t  // Stop Actor
    }
}

object JsonJournalActor {
  private val logger = Logger(getClass)

  private object Internal {
    final case class Commit(writtenLevel: Int)
  }

  private case class Written(eventSnapshots: Seq[Snapshot[AnyKeyedEvent]], replyTo: ActorRef, sender: ActorRef)
}

/*
  Transaktionsklammer:
    RS "begin" NL
    RS "end" NL
    Order [json1, json2, ...] (vielleicht schwer mit jq zu lesen)
  TODO Jeden Satz (jede Transaktion) mit einer Prüfsumme abschließen? Etwa java.util.zip.CRC32 (Adler-32?) oder Fletcher-32?
    RS checksum number NL
  Komplexe Metainfos könnten als Array angegeben werden.
   RS [ checksum number , ... ]
*/

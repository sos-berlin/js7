package com.sos.jobscheduler.shared.event.journal

import akka.actor.ActorRef
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.{DuplicateKeyException, Logger}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, KeyedEvent, Snapshot}
import com.sos.jobscheduler.shared.event.journal.JsonJournalMeta.Header
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer._
import java.nio.file.{Files, Path}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class JsonJournalRecoverer(meta: JsonJournalMeta, file: Path)
extends AutoCloseable with Iterator[Recovering]
{
  import meta.{convertInputStream, eventJsonFormat, isDeletedEvent, snapshotJsonFormat, snapshotToKey}

  private val stopwatch = new Stopwatch
  private val jsonIterator =
    if (Files.exists(file)) {
      logger.info(s"Recovering from journal file '$file' (${toMB(Files.size(file))})")
      new JsonFileIterator(Header, convertInputStream, file)
    } else {
      logger.info(s"No journal file '$file' left")
      JsonFileIterator.Empty
    }
  private val keyToActor = mutable.Map[Any, ActorRef]()
  private var buffer = readNext()
  private var closed = false
  private var snapshotCount = 0
  private var eventCount = 0

  def close() = {
    jsonIterator.close()
    if (!closed) {
      closed = true
      logSpeed()
      logger.debug(s"$snapshotCount snapshots and $eventCount events read")
    }
  }

  private def logSpeed() = {
    val duration = stopwatch.duration
    if (duration >= 1.s) {
      logger.debug("Speed: " + Stopwatch.Result(snapshotCount + eventCount, "snapshots+events", duration))
    }
  }

  def hasNext = {
    if (buffer.isEmpty) {
      buffer = readNext()
    }
    buffer.nonEmpty
  }

  def next = {
    hasNext
    val o = buffer.get
    buffer = None
    o
  }

  private def readNext(): Option[Recovering] =
    if (!jsonIterator.hasNext)
      None
    else {
      val jsValue = jsonIterator.next()
      if (eventJsonFormat canDeserialize jsValue.asJsObject) {
        eventCount += 1
        val eventSnapshot = jsValue.convertTo[Snapshot[AnyKeyedEvent]]
        val KeyedEvent(key, event) = eventSnapshot.value
        keyToActor.get(key) match {
          case None ⇒
            Some(RecoveringForUnknownKey(eventSnapshot))
          case Some(a) ⇒
            a ! JournalingActor.Input.RecoverFromEvent(eventSnapshot)
            if (isDeletedEvent(event)) {
              keyToActor -= key
              Some(RecoveringDeleted(eventSnapshot))
            } else
              Some(RecoveringChanged(eventSnapshot))
        }
      } else {
        snapshotCount += 1
        Some(RecoveringSnapshot(snapshotJsonFormat.read(jsValue)))
      }
    }

  def addActorForSnapshot(snapshot: Any, actorRef: ActorRef): Unit = {
    val key = snapshotToKey(snapshot)
    if (keyToActor isDefinedAt key) throw new DuplicateKeyException(s"Duplicate snapshot in journal file: '$key'")
    keyToActor += key → actorRef
    actorRef ! JournalingActor.Input.RecoverFromSnapshot(snapshot)
  }

  def addActorForFirstEvent(eventSnapshot: Snapshot[AnyKeyedEvent], actorRef: ActorRef): Unit = {
    val keyedEvent = eventSnapshot.value
    import keyedEvent.key
    if (keyToActor isDefinedAt key) throw new DuplicateKeyException(s"Duplicate key: '$key'")
    keyToActor += key → actorRef
    actorRef ! JournalingActor.Input.RecoverFromEvent(eventSnapshot)
  }

  //def startJournalAndFinishRecovery(parent: Actor, journalActor: ActorRef): Unit = {
  //  close()
  //  JsonJournalRecoverer.startJournalAndFinishRecovery(parent = parent, journalActor = journalActor, recoveredJournalingActors)
  //}

  def recoveredJournalingActors: RecoveredJournalingActors =
    RecoveredJournalingActors(keyToActor.toMap)
}

object JsonJournalRecoverer {
  private val logger = Logger(getClass)

  sealed trait Recovering
  final case class RecoveringSnapshot(snapshot: Any) extends Recovering
  sealed trait RecoveringEvent extends Recovering {
    def eventSnapshot: Snapshot[AnyKeyedEvent]
  }
  final case class RecoveringForUnknownKey(eventSnapshot: Snapshot[AnyKeyedEvent]) extends Recovering
  sealed trait RecoveringForKnownKey extends Recovering {
    def eventSnapshot: Snapshot[AnyKeyedEvent]
  }
  final case class RecoveringChanged(eventSnapshot: Snapshot[AnyKeyedEvent]) extends RecoveringForKnownKey
  final case class RecoveringDeleted(eventSnapshot: Snapshot[AnyKeyedEvent]) extends RecoveringForKnownKey

  //sealed trait RecoverResult
  //final case class AddActorForSnapshot(snapshot: Any, actorRef: ActorRef) extends RecoverResult
  //final case class AddActorForFirstEvent(eventSnapshot: Snapshot[AnyKeyedEvent], actorRef: ActorRef) extends RecoverResult
  //final case object Ignore extends RecoverResult
  //
  //def recover(meta: JsonJournalMeta, file: Path)(recover: PartialFunction[Recovered, RecoverResult]): RecoveredJournalingActors =
  //  autoClosing(new JsonJournalRecoverer(meta, file)) { journal ⇒
  //    for (recovered ← journal) {
  //      recover(recovered) match {
  //        case AddActorForSnapshot(snapshot, actorRef) ⇒
  //          journal.addActorForSnapshot(snapshot, actorRef)
  //        case AddActorForFirstEvent(eventSnapshot, actorRef) ⇒
  //          journal.addActorForFirstEvent(eventSnapshot, actorRef)
  //        case Ignore ⇒
  //      }
  //    }
  //    journal.recoveredJournalingActors
  //  }

  //def startJournalAndFinishRecovery(parent: Actor, journalActor: ActorRef, recoveredActors: RecoveredJournalingActors): Unit = {
  //  val actors = recoveredActors.keyToJournalingActor.values
  //  parent.context.actorOf(
  //    Props {
  //      new Actor {
  //        journalActor ! Journal.Input.Start(recoveredActors)
  //
  //        def receive = {
  //          case Journal.Output.Ready ⇒
  //            for (a ← actors) {
  //              a ! JournalingActor.Input.FinishRecovery
  //            }
  //            becomeWaitingForChildren(actors.size)
  //        }
  //
  //        private def becomeWaitingForChildren(n: Int): Unit = {
  //          if (n == 0) {
  //            context.parent ! Output.JournalIsReady(recoveredActors)
  //            context.stop(self)
  //          } else {
  //            context.become {
  //              case JournalingActor.Output.RecoveryFinished ⇒
  //                becomeWaitingForChildren(n - 1)
  //            }
  //          }
  //        }
  //      }
  //    },
  //    name = "JsonJournalRecoverer")
  //}

  private def toMB(size: Long): String = size match {
    case _ if size < 1000 * 1000 ⇒ "<1MB"
    case _ ⇒ (size + 999999) / (1000 * 1000) + "MB"
  }

  final case class RecoveredJournalingActors(keyToJournalingActor: Map[Any, ActorRef])

  //object Output {
  //  final case class JournalIsReady(recoveredJournalingActors: RecoveredJournalingActors)
  //}
}

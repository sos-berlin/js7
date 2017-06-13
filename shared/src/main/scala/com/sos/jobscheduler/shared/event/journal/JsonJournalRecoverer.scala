package com.sos.jobscheduler.shared.event.journal

import akka.actor.ActorRef
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichPartialFunction, RichThrowable, RichUnitPartialFunction}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.{DuplicateKeyException, Logger}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.shared.event.journal.JsonJournalMeta.Header
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer._
import java.nio.file.{Files, Path}
import scala.collection.mutable
import spray.json.JsValue

/**
  * @author Joacim Zschimmer
  */
abstract class JsonJournalRecoverer[E <: Event](meta: JsonJournalMeta[E], file: Path) {

  import meta.{convertInputStream, eventJsonFormat, isDeletedEvent, snapshotJsonFormat, snapshotToKey}

  def recoverSnapshot: PartialFunction[Any, Unit]
  def recoverNewKey: PartialFunction[Stamped[AnyKeyedEvent], Unit]
  def onChangedRecovered: PartialFunction[Stamped[AnyKeyedEvent], Unit] = PartialFunction.empty
  def onDeletedRecovered: PartialFunction[Stamped[AnyKeyedEvent], Unit] = PartialFunction.empty

  private val stopwatch = new Stopwatch
  private val keyToActor = mutable.Map[Any, ActorRef]()
  private var lastEventId: EventId = EventId.BeforeFirst
  private var snapshotCount = 0
  private var eventCount = 0

  final def recoverAllAndSendTo(journalActor: ActorRef)(implicit sender: ActorRef): Unit = {
    try
      autoClosing(newJsonIterator()) { jsonIterator ⇒
        while (jsonIterator.hasNext) {
          recoverJsValue(jsonIterator.next())
        }
      }
    catch {
      case t: Exception if meta.isIncompleteException(t) ⇒
        logger.info(s"Journal has not been completed. " + errorClause(t))
      case t: Exception if meta.isCorruptException(t) ⇒
        logger.warn(s"Journal is corrupt or has not been completed. " + errorClause(t))
    }
    logSomething()
    journalActor ! JsonJournalActor.Input.Start(recoveredJournalingActors)
  }

  private def newJsonIterator(): AutoCloseable with Iterator[JsValue] =
    if (Files.exists(file)) {
      logger.info(s"Recovering from journal file '$file' (${toMB(Files.size(file))})")
      new JsonFileIterator(Header, convertInputStream, file)
    } else {
      logger.info(s"No journal file '$file' left")
      JsonFileIterator.Empty
    }

  private def recoverJsValue(jsValue: JsValue): Unit = {
    if (eventJsonFormat canDeserialize jsValue.asJsObject) {
      eventCount += 1
      val stamped = jsValue.convertTo[Stamped[KeyedEvent[E]]]
      if (stamped.eventId <= lastEventId)
        sys.error(s"Journal is corrupt, EventIds are out of order: ${EventId.toString(lastEventId)} >= ${EventId.toString(stamped.eventId)}")
      lastEventId = stamped.eventId
      val KeyedEvent(key, event) = stamped.value
      keyToActor.get(key) match {
        case None ⇒
          recoverNewKey.getOrElse(stamped,
            sys.error(s"Uncoverable event for a new key in journal file '$file': $stamped"))
        case Some(a) ⇒
          a ! KeyedJournalingActor.Input.RecoverFromEvent(stamped)   // TODO Possible Actor mailbox overflow
          if (isDeletedEvent(event)) {
            keyToActor -= key
            onDeletedRecovered.callIfDefined(stamped)
          } else
            onChangedRecovered.callIfDefined(stamped)
        }
      } else {
        snapshotCount += 1
        val snapshot = snapshotJsonFormat.read(jsValue)
        recoverSnapshot.getOrElse(snapshot,
          sys.error(s"Uncoverable snapshot in journal file '$file': $snapshot"))
      }
    }

  private def errorClause(t: Throwable) =
    s"Assuming sudden termination, using $snapshotCount snapshots and $eventCount events until ${EventId.toString(lastEventId)}. ${t.toStringWithCauses}"

  private def logSomething(): Unit = {
    if (stopwatch.duration >= 1.s) {
      logger.debug("Speed: " + stopwatch.itemsPerSecondString(snapshotCount + eventCount, "snapshots+events"))
    }
    if (eventCount > 0) {
      logger.info(s"Recovered last EventId is ${EventId.toString(lastEventId)} ($snapshotCount snapshots and $eventCount events read)")
    }
  }

  protected def recoverActorForSnapshot(snapshot: Any, actorRef: ActorRef): Unit = {
    val key = snapshotToKey(snapshot)
    if (keyToActor isDefinedAt key) throw new DuplicateKeyException(s"Duplicate snapshot in journal file: '$key'")
    keyToActor += key → actorRef
    actorRef ! KeyedJournalingActor.Input.RecoverFromSnapshot(snapshot)
  }

  protected def recoverActorForNewKey(stampedEvent: Stamped[AnyKeyedEvent], actorRef: ActorRef): Unit = {
    val keyedEvent = stampedEvent.value
    import keyedEvent.key
    if (keyToActor isDefinedAt key) throw new DuplicateKeyException(s"Duplicate key: '$key'")
    keyToActor += key → actorRef
    actorRef ! KeyedJournalingActor.Input.RecoverFromEvent(stampedEvent)
  }

  //def startJournalAndFinishRecovery(parent: Actor, journalActor: ActorRef): Unit = {
  //  close()
  //  JsonJournalRecoverer.startJournalAndFinishRecovery(parent = parent, journalActor = journalActor, recoveredJournalingActors)
  //}

  final def recoveredJournalingActors: RecoveredJournalingActors =
    RecoveredJournalingActors(keyToActor.toMap)
}

object JsonJournalRecoverer {
  private val logger = Logger(getClass)

  //sealed trait RecoverResult
  //final case class AddActorForSnapshot(snapshot: Any, actorRef: ActorRef) extends RecoverResult
  //final case class AddActorForFirstEvent(stampedEvent: Stamped[AnyKeyedEvent], actorRef: ActorRef) extends RecoverResult
  //final case object Ignore extends RecoverResult
  //
  //def recover(meta: JsonJournalMeta, file: Path)(recover: PartialFunction[Recovered, RecoverResult]): RecoveredJournalingActors =
  //  autoClosing(new JsonJournalRecoverer(meta, file)) { journal ⇒
  //    for (recovered ← journal) {
  //      recover(recovered) match {
  //        case AddActorForSnapshot(stamped, actorRef) ⇒
  //          journal.recoverActorForSnapshot(stamped, actorRef)
  //        case AddActorForFirstEvent(stampedEvent, actorRef) ⇒
  //          journal.recoverActorForNewKey(stampedEvent, actorRef)
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
  //        journalActor ! JsonJournalActor.Input.Start(recoveredActors)
  //
  //        def receive = {
  //          case JsonJournalActor.Output.Ready ⇒
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

  private[journal] def toMB(size: Long): String = size match {
    case _ if size < 1000 * 1000 ⇒ "<1MB"
    case _ ⇒ (size + 999999) / (1000 * 1000) + "MB"
  }

  final case class RecoveredJournalingActors(keyToJournalingActor: Map[Any, ActorRef])

  //object Output {
  //  final case class JournalIsReady(recoveredJournalingActors: RecoveredJournalingActors)
  //}
}

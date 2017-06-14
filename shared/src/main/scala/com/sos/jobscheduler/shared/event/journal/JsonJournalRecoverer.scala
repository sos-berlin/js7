package com.sos.jobscheduler.shared.event.journal

import akka.actor.{Actor, ActorContext, ActorRef, Props}
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
trait JsonJournalRecoverer[E <: Event] {

  protected val jsonJournalMeta: JsonJournalMeta[E]
  protected def journalFile: Path

  import jsonJournalMeta.{convertInputStream, eventJsonFormat, isDeletedEvent, snapshotJsonFormat, snapshotToKey}

  def recoverSnapshot: PartialFunction[Any, Unit]
  def recoverNewKey: PartialFunction[Stamped[AnyKeyedEvent], Unit]
  def onChangedRecovered: PartialFunction[Stamped[AnyKeyedEvent], Unit] = PartialFunction.empty
  def onDeletedRecovered: PartialFunction[Stamped[AnyKeyedEvent], Unit] = PartialFunction.empty

  private val stopwatch = new Stopwatch
  private val keyToActor = mutable.Map[Any, ActorRef]()
  private var lastEventId: EventId = EventId.BeforeFirst
  private var snapshotCount = 0
  private var eventCount = 0

  final def recoverAllAndSendTo(journalActor: ActorRef)(implicit context: ActorContext, sender: ActorRef): Unit = {
    try
      autoClosing(newJsonIterator()) { jsonIterator ⇒
        while (jsonIterator.hasNext) {
          recoverJsValue(jsonIterator.next())
        }
      }
    catch {
      case t: Exception if jsonJournalMeta.isIncompleteException(t) ⇒
        logger.info(s"Journal has not been completed. " + errorClause(t))
      case t: Exception if jsonJournalMeta.isCorruptException(t) ⇒
        logger.warn(s"Journal is corrupt or has not been completed. " + errorClause(t))
    }
    logSomething()
    JsonJournalRecoverer.startJournalAndFinishRecovery(context, journalActor = journalActor, recoveredJournalingActors)
  }

  private def newJsonIterator(): AutoCloseable with Iterator[JsValue] =
    if (Files.exists(journalFile)) {
      logger.info(s"Recovering from journal journalFile '$journalFile' (${toMB(Files.size(journalFile))})")
      new JsonFileIterator(Header, convertInputStream, journalFile)
    } else {
      logger.info(s"No journal journalFile '$journalFile' left")
      JsonFileIterator.Empty
    }

  private def recoverJsValue(jsValue: JsValue): Unit = {
    if (eventJsonFormat canDeserialize jsValue.asJsObject) {
      recoverEventJsValue(jsValue)
    } else {
      recoverSnapshotJsValue(jsValue)
    }
  }

  private def recoverSnapshotJsValue(jsValue: JsValue): Unit = {
    snapshotCount += 1
    val snapshot = snapshotJsonFormat.read(jsValue)
    recoverSnapshot.getOrElse(snapshot,
      sys.error(s"Uncoverable snapshot in journal journalFile '$journalFile': $snapshot"))
  }

  private def recoverEventJsValue(jsValue: JsValue): Unit = {
    eventCount += 1
    val stamped = jsValue.convertTo[Stamped[KeyedEvent[E]]]
    if (stamped.eventId <= lastEventId)
      sys.error(s"Journal is corrupt, EventIds are out of order: ${EventId.toString(stamped.eventId)} follows ${EventId.toString(lastEventId)}")
    lastEventId = stamped.eventId
    val KeyedEvent(key, event) = stamped.value
    keyToActor.get(key) match {
      case None ⇒
        recoverNewKey.getOrElse(stamped,
          sys.error(s"Uncoverable event for a new key in journal journalFile '$journalFile': $stamped"))
      case Some(a) ⇒
        a ! KeyedJournalingActor.Input.RecoverFromEvent(stamped)   // TODO Possible Actor mailbox overflow
        if (isDeletedEvent(event)) {
          keyToActor -= key
          onDeletedRecovered.callIfDefined(stamped)
        } else
          onChangedRecovered.callIfDefined(stamped)
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
    if (keyToActor isDefinedAt key) throw new DuplicateKeyException(s"Duplicate snapshot in journal journalFile: '$key'")
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

  final def recoveredJournalingActors: RecoveredJournalingActors =
    RecoveredJournalingActors(keyToActor.toMap)
}

object JsonJournalRecoverer {
  private val logger = Logger(getClass)

  def startJournalAndFinishRecovery(parentContext: ActorContext, journalActor: ActorRef, recoveredActors: RecoveredJournalingActors): Unit = {
    val actors = recoveredActors.keyToJournalingActor.values
    val actorToKey = (recoveredActors.keyToJournalingActor map { case (k, a) ⇒ a → k })
    parentContext.actorOf(
      Props {
        new Actor {
          journalActor ! JsonJournalActor.Input.Start(recoveredActors)

          def receive = {
            case JsonJournalActor.Output.Ready ⇒
              for (a ← actors) {
                a ! KeyedJournalingActor.Input.FinishRecovery
              }
              logger.debug(s"Awaiting RecoveryFinish of ${actors.size} actors")
              becomeWaitingForChildren(actors.size)
          }

          private def becomeWaitingForChildren(n: Int): Unit = {
            if (n == 0) {
              context.parent ! Output.JournalIsReady
              context.stop(self)
            } else {
              context.become {
                case KeyedJournalingActor.Output.RecoveryFinished ⇒
                  logger.debug(s"${n - 1} actors left: Actor has RecoveryFinished: '${actorToKey(sender())}'")
                  becomeWaitingForChildren(n - 1)

                case msg if actorToKey contains sender() ⇒
                  context.parent.forward(msg)  // For example OrderActor.Output.RecoveryFinished
              }
            }
          }
        }
      },
      name = "JsonJournalRecoverer")
  }

  private[journal] def toMB(size: Long): String = size match {
    case _ if size < 1000 * 1000 ⇒ "<1MB"
    case _ ⇒ (size + 999999) / (1000 * 1000) + "MB"
  }

  final case class RecoveredJournalingActors(keyToJournalingActor: Map[Any, ActorRef])

  object Output {
    case object JournalIsReady
  }
}

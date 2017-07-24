package com.sos.jobscheduler.shared.event.journal

import akka.actor.{Actor, ActorContext, ActorRef, ActorRefFactory, Props}
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichPartialFunction, RichThrowable}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.shared.event.journal.JsonJournalMeta.Header
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer._
import com.sos.jobscheduler.shared.event.journal.Utils.toMB
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal
import spray.json.JsValue

/**
  * @author Joacim Zschimmer
  */
trait JsonJournalRecoverer[E <: Event] {

  protected val jsonJournalMeta: JsonJournalMeta[E]
  protected def journalFile: Path
  protected def recoverSnapshot: PartialFunction[Any, Unit]
  protected def recoverEvent: PartialFunction[Stamped[AnyKeyedEvent], Unit]
  protected def recoveredJournalingActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty

  import jsonJournalMeta.{convertInputStream, eventJsonFormat, snapshotJsonFormat}

  private val stopwatch = new Stopwatch
  private var lastEventId: EventId = EventId.BeforeFirst
  private var snapshotCount = 0
  private var eventCount = 0

  final def recoverAllAndTransferTo(journalActor: ActorRef)(implicit context: ActorContext): Unit = {
    recoverAll()
    startJournalAndFinishRecovery(context, journalActor = journalActor, recoveredJournalingActors)
  }

  final def recoverAll(): Unit = {
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
      sys.error(s"Unrecoverable snapshot in journal journalFile '$journalFile': $snapshot"))
  }

  private def recoverEventJsValue(jsValue: JsValue): Unit = {
    eventCount += 1
    val stamped = jsValue.convertTo[Stamped[KeyedEvent[E]]]
    if (stamped.eventId <= lastEventId)
      sys.error(s"Journal is corrupt, EventIds are out of order: ${EventId.toString(stamped.eventId)} follows ${EventId.toString(lastEventId)}")
    lastEventId = stamped.eventId
    try recoverEvent.getOrElse(stamped, sys.error("Not handled"))
    catch { case NonFatal(t) ⇒
      throw new RuntimeException(s"Unrecoverable event in journal '$journalFile': $stamped: $t", t)
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
}

object JsonJournalRecoverer {
  private val logger = Logger(getClass)

  def startJournalAndFinishRecovery(actorRefFactory: ActorRefFactory, journalActor: ActorRef, recoveredActors: RecoveredJournalingActors): Unit = {
    val actors = recoveredActors.keyToJournalingActor.values
    val actorToKey = (recoveredActors.keyToJournalingActor map { case (k, a) ⇒ a → k })
    actorRefFactory.actorOf(
      Props {
        new Actor {
          journalActor ! JsonJournalActor.Input.Start(recoveredActors)

          def receive = {
            case JsonJournalActor.Output.Ready ⇒
              for (a ← actors) {
                a ! KeyedJournalingActor.Input.FinishRecovery
              }
              logger.debug(s"Awaiting RecoveryFinished of ${actors.size} actors")
              becomeWaitingForChildren(actors.size)
          }

          private def becomeWaitingForChildren(n: Int): Unit = {
            if (n == 0) {
              logger.debug(s"JournalIsReady")
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
      name = "JsonJournalActorRecoverer")
  }

  object Output {
    case object JournalIsReady
  }
}

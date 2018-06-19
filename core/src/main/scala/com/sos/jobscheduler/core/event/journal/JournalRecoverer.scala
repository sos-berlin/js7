package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.ByteUnits.toMB
import com.sos.jobscheduler.common.utils.Exceptions.wrapException
import com.sos.jobscheduler.common.utils.untilNoneIterator
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import java.nio.file.Files.exists
import java.nio.file.{Files, Path}
import scala.concurrent.blocking

/**
  * @author Joacim Zschimmer
  */
trait JournalRecoverer[E <: Event] {

  protected val journalMeta: JournalMeta[E]
  protected def journalFile: Path
  protected def recoverSnapshot: PartialFunction[Any, Unit]
  protected def recoverEvent: PartialFunction[Stamped[KeyedEvent[E]], Unit]

  private var _eventsAcceptedUntil = EventId.BeforeFirst
  private var _lastEventId = EventId.BeforeFirst
  private lazy val logger = Logger.withPrefix[JournalRecoverer[_]](journalFile.getFileName.toString)
  protected lazy val hasJournal = exists(journalFile)

  final def recoverAll(): Unit =
    if (!hasJournal) {
      logger.info(s"No journal journalFile '$journalFile' left")
    } else
      blocking {  // May take a long time
        logger.info(s"Recovering from journal journalFile '$journalFile' (${toMB(Files.size(journalFile))})")
        autoClosing(new JournalReader(journalMeta, journalFile)) { journalReader ⇒
          untilNoneIterator { journalReader.recoverNext() } foreach {
            case JournalReader.RecoveredSnapshot(snapshot) ⇒
              wrapException(s"Error recovering snapshot ${snapshot.getClass.scalaName}") {
                recoverSnapshot(snapshot)
              }
            case JournalReader.RecoveredEvent(stampedEvent) ⇒
              wrapException(s"Error recovering event ${EventId.toString(stampedEvent.eventId)} ${stampedEvent.value.getClass.scalaName} '${stampedEvent.value.key}'") {
                recoverEvent(stampedEvent)
              }
          }
          _eventsAcceptedUntil = journalReader.eventsAcceptedUntil
          _lastEventId = journalReader.lastReadEventId
          journalReader.logStatistics()
        }
      }

  def startJournalAndFinishRecovery(
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty,
    eventReaderProvider: Option[JournalEventReaderProvider[E]] = None)
    (implicit actorRefFactory: ActorRefFactory)
  =
    JournalRecoverer.startJournalAndFinishRecovery[E](journalActor, recoveredActors, eventReaderProvider,
      eventsAcceptedUntil = _eventsAcceptedUntil, lastEventId = _lastEventId)

  val lastRecoveredEventId = _lastEventId
}

object JournalRecoverer {
  private val logger = Logger(getClass)

  private def startJournalAndFinishRecovery[E <: Event](
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty,
    eventReaderProvider: Option[JournalEventReaderProvider[E]] = None,
    eventsAcceptedUntil: EventId,
    lastEventId: EventId)
    (implicit actorRefFactory: ActorRefFactory)
  : Unit = {
    val actors = recoveredActors.keyToJournalingActor.values
    val actorToKey = recoveredActors.keyToJournalingActor map { case (k, a) ⇒ a → k }
    actorRefFactory.actorOf(
      Props {
        new Actor {
          journalActor ! JournalActor.Input.Start(recoveredActors, eventReaderProvider, eventsAcceptedUntil = eventsAcceptedUntil, lastEventId = lastEventId)

          def receive = {
            case JournalActor.Output.Ready ⇒
              for (a ← actors) {
                a ! KeyedJournalingActor.Input.FinishRecovery
              }
              logger.debug(s"Awaiting RecoveryFinished from ${actors.size} actors")
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
                  logger.trace(s"${n - 1} actors left: Actor has RecoveryFinished: ${actorToKey(sender())}'")
                  becomeWaitingForChildren(n - 1)

                case msg if actorToKey contains sender() ⇒
                  context.parent.forward(msg)  // For example OrderActor.Output.RecoveryFinished
              }
            }
          }
        }
      },
      name = "JournalActorRecoverer")
  }

  object Output {
    case object JournalIsReady
  }
}

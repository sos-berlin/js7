package com.sos.jobscheduler.core.event.journal.recover

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.Exceptions.wrapException
import com.sos.jobscheduler.common.utils.untilNoneIterator
import com.sos.jobscheduler.core.event.journal.data.{JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.recover.JournalRecovererReader.{AllSnapshotsRecovered, RecoveredEvent, RecoveredSnapshot}
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.core.event.journal.{JournalActor, KeyedJournalingActor}
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import scala.concurrent.blocking

/**
  * @author Joacim Zschimmer
  */
trait JournalRecoverer[E <: Event] {

  protected val journalMeta: JournalMeta[E]
  protected def recoverSnapshot: PartialFunction[Any, Unit]
  protected def recoverEvent: PartialFunction[Stamped[KeyedEvent[E]], Unit]
  protected def onAllSnapshotRecovered(): Unit = {}

  private var _lastEventId = EventId.BeforeFirst
  protected lazy val journalFileOption = JournalFiles.currentFile(journalMeta.fileBase).toOption

  protected final def hasJournal = journalFileOption.isDefined

  final def recoverAll(): Unit =
    journalFileOption match {
      case None ⇒
      case Some(file) ⇒
        blocking {  // May take a long time
          autoClosing(new JournalRecovererReader(journalMeta, file)) { journalReader ⇒
            untilNoneIterator { journalReader.recoverNext() } foreach {
              case RecoveredSnapshot(snapshot) ⇒
                wrapException(s"Error recovering snapshot ${snapshot.getClass.scalaName}") {
                  recoverSnapshot(snapshot)
                }

              case AllSnapshotsRecovered ⇒
                onAllSnapshotRecovered()

              case RecoveredEvent(stampedEvent) ⇒
                wrapException(s"Error recovering event ${EventId.toString(stampedEvent.eventId)} ${stampedEvent.value.toShortString}") {
                  recoverEvent(stampedEvent)
                }
            }
            _lastEventId = journalReader.lastReadEventId
            journalReader.logStatistics()
          }
        }
      }

  final def startJournalAndFinishRecovery(
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty,
    journalingObserver: Option[JournalingObserver] = None)
    (implicit actorRefFactory: ActorRefFactory)
  =
    JournalRecoverer.startJournalAndFinishRecovery[E](journalActor, recoveredActors, journalingObserver, lastEventId = _lastEventId)

  final def lastRecoveredEventId = _lastEventId
}

object JournalRecoverer {
  private val logger = Logger(getClass)

  private def startJournalAndFinishRecovery[E <: Event](
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty,
    observer: Option[JournalingObserver] = None,
    lastEventId: EventId)
    (implicit actorRefFactory: ActorRefFactory)
  : Unit = {
    val actors = recoveredActors.keyToJournalingActor.values
    val actorToKey = recoveredActors.keyToJournalingActor map { case (k, a) ⇒ a → k }
    actorRefFactory.actorOf(
      Props {
        new Actor {
          journalActor ! JournalActor.Input.Start(recoveredActors, observer, lastEventId = lastEventId)

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

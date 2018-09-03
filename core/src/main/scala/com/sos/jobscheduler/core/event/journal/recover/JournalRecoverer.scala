package com.sos.jobscheduler.core.event.journal.recover

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.utils.Exceptions.wrapException
import com.sos.jobscheduler.core.event.journal.data.{JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer._
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.core.event.journal.{JournalActor, KeyedJournalingActor}
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import scala.concurrent.blocking

/**
  * @author Joacim Zschimmer
  */
trait JournalRecoverer[E <: Event] {

  private val stopwatch = new Stopwatch
  protected val journalMeta: JournalMeta[E]
  protected def recoverSnapshot: PartialFunction[Any, Unit]
  protected def recoverEvent: PartialFunction[Stamped[KeyedEvent[E]], Unit]
  protected def onAllSnapshotRecovered(): Unit = {}

  private var _lastEventId = EventId.BeforeFirst
  private var snapshotCount, eventCount = 0L
  private var _totalEventCount = 0L
  protected lazy val journalFileOption = JournalFiles.currentFile(journalMeta.fileBase).toOption

  protected final def hasJournal = journalFileOption.isDefined

  final def recoverAll(): Unit =
    for (file ← journalFileOption) {
      blocking {  // May take a long time
        autoClosing(new JournalReader(journalMeta, file)) { journalReader ⇒
          recoverSnapshots(journalReader)
          onAllSnapshotRecovered()
          recoverEvents(journalReader)
          _lastEventId = journalReader.eventId
          _totalEventCount = journalReader.totalEventCount
          logStatistics()
        }
      }
    }

  private def recoverSnapshots(journalReader: JournalReader[E]): Unit =
    for (snapshot ← journalReader.nextSnapshots()) {
      wrapException(s"Error recovering snapshot ${snapshot.getClass.scalaName}") {
        snapshotCount += 1
        recoverSnapshot(snapshot)
      }
    }

  private def recoverEvents(journalReader: JournalReader[E]): Unit =
    for (stampedEvent ← journalReader.nextEvents()) {
      wrapException(s"Error recovering event ${EventId.toString(stampedEvent.eventId)} ${stampedEvent.value.toShortString}") {
        eventCount += 1
        recoverEvent(stampedEvent)
      }
    }

  private def logStatistics(): Unit = {
    if (stopwatch.duration >= 1.s) {
      logger.debug(stopwatch.itemsPerSecondString(snapshotCount + eventCount, "snapshots+events") + " read")
    }
    if (eventCount > 0) {
      val time = EventId.toDateTimeString(_lastEventId)
      val age = (now - EventId.toTimestamp(_lastEventId)).withNanos(0).pretty
      logger.info(s"Recovered last EventId is ${_lastEventId} of $time $age ago " +
        s"($snapshotCount snapshot elements and $eventCount events read in ${stopwatch.duration.pretty})")
    }
  }

  final def startJournalAndFinishRecovery(
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty,
    journalingObserver: Option[JournalingObserver] = None)
    (implicit actorRefFactory: ActorRefFactory)
  =
    JournalRecoverer.startJournalAndFinishRecovery[E](journalActor, recoveredActors, journalingObserver, lastEventId = _lastEventId,
      totalEventCount = _totalEventCount)

  final def lastRecoveredEventId = _lastEventId
}

object JournalRecoverer {
  private val logger = Logger(getClass)

  private def startJournalAndFinishRecovery[E <: Event](
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty,
    observer: Option[JournalingObserver] = None,
    lastEventId: EventId,
    totalEventCount: Long)
    (implicit actorRefFactory: ActorRefFactory)
  : Unit = {
    val actors = recoveredActors.keyToJournalingActor.values
    val actorToKey = recoveredActors.keyToJournalingActor map { case (k, a) ⇒ a → k }
    actorRefFactory.actorOf(
      Props {
        new Actor {
          journalActor ! JournalActor.Input.Start(recoveredActors, observer, lastEventId = lastEventId, totalEventCount = totalEventCount)

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

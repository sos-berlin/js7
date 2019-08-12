package com.sos.jobscheduler.core.event.journal.recover

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.common.utils.Exceptions.wrapException
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer._
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.core.event.journal.{JournalActor, KeyedJournalingActor}
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, KeyedEvent, Stamped}
import java.nio.file.Files
import java.util.UUID.randomUUID
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
trait JournalRecoverer[E <: Event]
{
  protected def journalMeta: JournalMeta[E]
  protected def expectedJournalId: Option[JournalId]
  protected def recoverSnapshot: PartialFunction[Any, Unit]
  protected def recoverEvent: PartialFunction[Stamped[KeyedEvent[E]], Unit]
  protected def onAllSnapshotRecovered(): Unit = {}

  private val stopwatch = new Stopwatch
  private val journalHeaderOnce = SetOnce[JournalHeader]
  private var _lastEventId = EventId.BeforeFirst
  private var snapshotCount, eventCount = 0L
  private var _totalEventCount = 0L
  protected lazy val journalFileOption = JournalFiles.currentFile(journalMeta.fileBase).toOption

  final def hasJournal = journalFileOption.isDefined

  final def recoverAll(): Unit =
    journalFileOption match {
      case None =>
        val journalId = expectedJournalId getOrElse JournalId.random()
        logger.info(s"No journal to recover - a new journal '${journalMeta.fileBase.getFileName}' with journalId=${journalId.string} will be started")
        journalHeaderOnce := JournalHeader.initial(journalId)

      case Some(file) =>
        logger.info(s"Recovering from file ${file.getFileName} (${toKBGB(Files.size(file))})")
        // TODO Use HistoricEventReader (and build JournalIndex only once, and reuse it for event reading)
        autoClosing(new JournalReader(journalMeta, expectedJournalId, file)) { journalReader =>
          journalHeaderOnce := journalReader.journalHeader
          recoverSnapshots(journalReader)
          onAllSnapshotRecovered()
          recoverEvents(journalReader)
          _lastEventId = journalReader.eventId
          _totalEventCount = journalReader.totalEventCount
          logStatistics()
        }
    }

  private def recoverSnapshots(journalReader: JournalReader[E]): Unit =
    for (snapshot <- journalReader.nextSnapshotsWithoutJournalHeader()) {
      wrapException(s"Error recovering snapshot ${snapshot.getClass.scalaName}") {
        snapshotCount += 1
        recoverSnapshot(snapshot)
      }
    }

  private def recoverEvents(journalReader: JournalReader[E]): Unit =
    for (stampedEvent <- journalReader.nextEvents()) {
      wrapException(s"Error while recovering event ${EventId.toString(stampedEvent.eventId)} ${stampedEvent.value.toShortString}") {
        eventCount += 1
        recoverEvent(stampedEvent)
      }
    }

  private def logStatistics(): Unit = {
    if (stopwatch.duration >= 1.s) {
      logger.debug(stopwatch.itemsPerSecondString(snapshotCount + eventCount, "snapshots+events") + " read")
    }
    if (eventCount > 0) {
      val age = (Timestamp.now - EventId.toTimestamp(_lastEventId)).withMillis(0).pretty
      logger.info(s"Recovered last EventId is ${EventId.toString(_lastEventId)}, issued $age ago " +
        s"($snapshotCount snapshot elements and $eventCount events read in ${stopwatch.duration.pretty})")
    }
  }

  final def startJournalAndFinishRecovery(
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty,
    journalingObserver: Option[JournalingObserver] = None)
    (implicit actorRefFactory: ActorRefFactory)
  =
    JournalRecoverer.startJournalAndFinishRecovery[E](journalActor, recoveredActors, journalingObserver, journalHeader)

  final lazy val journalHeader = {
    val lastTimestamp =
      if (_lastEventId == EventId.BeforeFirst) journalHeaderOnce().timestamp
      else EventId.toTimestamp(_lastEventId)
    journalHeaderOnce().copy(
      eventId = _lastEventId,
      totalEventCount = _totalEventCount,
      timestamp = lastTimestamp,
      totalRunningTime = journalHeaderOnce().totalRunningTime + (lastTimestamp - journalHeaderOnce().timestamp))
  }

  final def lastRecoveredEventId = _lastEventId
}

object JournalRecoverer {
  private val logger = Logger(getClass)

  private def startJournalAndFinishRecovery[E <: Event](
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors,
    observer: Option[JournalingObserver],
    recoveredJournalHeader: JournalHeader)
    (implicit actorRefFactory: ActorRefFactory)
  : Unit = {
    val actors = recoveredActors.keyToJournalingActor.values
    val actorToKey = recoveredActors.keyToJournalingActor map { case (k, a) => a -> k }
    actorRefFactory.actorOf(
      Props {
        new Actor {
          journalActor ! JournalActor.Input.Start(recoveredActors, observer, recoveredJournalHeader)

          def receive = {
            case JournalActor.Output.Ready(journalHeader, runningSince) =>
              for (a <- actors) {
                a ! KeyedJournalingActor.Input.FinishRecovery
              }
              logger.debug(s"Awaiting RecoveryFinished from ${actors.size} actors")
              becomeWaitingForChildren(journalHeader, runningSince, actors.size)
          }

          private def becomeWaitingForChildren(journalHeader: JournalHeader, runningSince: Deadline, n: Int): Unit = {
            if (n == 0) {
              logger.debug(s"JournalIsReady")
              context.parent ! Output.JournalIsReady(journalHeader, runningSince)
              context.stop(self)
            } else {
              context.become {
                case KeyedJournalingActor.Output.RecoveryFinished =>
                  logger.trace(s"${n - 1} actors left: Actor has RecoveryFinished: ${actorToKey(sender())}'")
                  becomeWaitingForChildren(journalHeader, runningSince, n - 1)

                case msg if actorToKey contains sender() =>
                  context.parent.forward(msg)  // For example OrderActor.Output.RecoveryFinished
              }
            }
          }
        }
      },
      name = "JournalActorRecoverer")
  }

  object Output {
    final case class JournalIsReady(journalHeader: JournalHeader, runningSince: Deadline)
  }
}

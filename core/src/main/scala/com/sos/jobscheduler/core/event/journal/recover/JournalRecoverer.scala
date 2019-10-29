package com.sos.jobscheduler.core.event.journal.recover

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.common.utils.Exceptions.wrapException
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer._
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.core.event.journal.{JournalActor, KeyedJournalingActor}
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, KeyedEvent, Stamped}
import java.nio.file.{Files, Path}
import scala.concurrent.duration._

// TODO Replace this class by JournalStateRecoverer

/**
  * @author Joacim Zschimmer
  */
trait JournalRecoverer
{
  protected def journalMeta: JournalMeta
  protected def expectedJournalId: Option[JournalId]
  protected def recoverSnapshot: PartialFunction[Any, Unit]
  protected def recoverEvent: PartialFunction[Stamped[KeyedEvent[Event]], Unit]
  protected def onAllSnapshotRecovered(): Unit = {}

  private val stopwatch = new Stopwatch
  private var maybeJournalHeader: Option[JournalHeader] = None
  private var _lastEventId = EventId.BeforeFirst
  private var _filePosition = 0L
  private var snapshotCount, eventCount = 0L
  private var _totalEventCount = 0L
  protected lazy val journalFileOption = JournalFiles.currentFile(journalMeta.fileBase).toOption

  final def hasJournal = journalFileOption.isDefined

  final def recoverAll(): Unit =
    journalFileOption match {
      case None =>
        logger.info(s"No journal to recover - a new journal '${journalMeta.fileBase.getFileName}'${expectedJournalId.fold("")(o => s" with journalId=$o")} will be started")

      case Some(file) =>
        logger.info(s"Recovering from file ${file.getFileName} (${toKBGB(Files.size(file))})")
        // TODO Use HistoricEventReader (and build JournalIndex only once, and reuse it for event reading)
        autoClosing(new JournalReader(journalMeta, expectedJournalId, file)) { journalReader =>
          maybeJournalHeader = Some(journalReader.journalHeader)
          recoverSnapshots(journalReader)
          onAllSnapshotRecovered()
          recoverEvents(journalReader)
          _lastEventId = journalReader.eventId
          _totalEventCount = journalReader.totalEventCount
          logStatistics()
        }
    }

  private def recoverSnapshots(journalReader: JournalReader): Unit =
    for (snapshot <- journalReader.nextSnapshotsWithoutJournalHeader()) {
      wrapException(s"Error recovering snapshot ${snapshot.getClass.scalaName}") {
        snapshotCount += 1
        recoverSnapshot(snapshot)
      }
    }

  private def recoverEvents(journalReader: JournalReader): Unit =
    for (stampedEvent <- journalReader.nextEvents()) {
      wrapException(s"Error while recovering event ${EventId.toString(stampedEvent.eventId)} ${stampedEvent.value.toShortString}") {
        eventCount += 1
        recoverEvent(stampedEvent)
        _filePosition = journalReader.position
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

  //def startJournal(journalActor: ActorRef @@ JournalActor.type, journalingObserver: Option[JournalEventWatch[E]]): Task[Completed] =
  //  Task.deferFutureAction(implicit s =>
  //    (journalActor ? JournalActor.Input.Start(RecoveredJournalingActors.Empty, journalingObserver, journalHeader))(journalMeta.askTimeout)
  //      .map { case _: JournalActor.Output.Ready => Completed })

  final def startJournalAndFinishRecovery(
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty,
    journalingObserver: Option[JournalingObserver] = None)
    (implicit actorRefFactory: ActorRefFactory)
  =
    JournalRecoverer.startJournalAndFinishRecovery[Event](journalActor, recoveredActors, journalingObserver, expectedJournalId, recoveredJournalHeader)

  final def journalId = maybeJournalHeader.map(_.journalId)

  /** With recovery time added. */
  private[event] final lazy val totalRunningTime: FiniteDuration =
    if (_lastEventId == EventId.BeforeFirst)
      Duration.Zero
    else
      maybeJournalHeader.fold(Duration.Zero)(o => o.totalRunningTime + (lastTimestamp - o.timestamp))

  /** Timestamp of the last event */
  private lazy val lastTimestamp: Timestamp =
    if (_lastEventId == EventId.BeforeFirst)
      Timestamp.now
    else
      EventId.toTimestamp(_lastEventId)

  private lazy val recoveredJournalHeader: Option[JournalHeader] =
    maybeJournalHeader.map(_.copy(
      eventId = _lastEventId,
      totalEventCount = _totalEventCount,
      timestamp = lastTimestamp,
      totalRunningTime = totalRunningTime))

  final def lastRecoveredEventId = _lastEventId

  final def positionAndFile: Option[PositionAnd[Path]] =
    journalFileOption.map(PositionAnd(_filePosition, _))
}

object JournalRecoverer {
  private val logger = Logger(getClass)

  private[recover] def startJournalAndFinishRecovery[E <: Event](
    journalActor: ActorRef,
    recoveredActors: RecoveredJournalingActors,
    observer: Option[JournalingObserver],
    expectedJournalId: Option[JournalId],
    recoveredJournalHeader: Option[JournalHeader])
    (implicit actorRefFactory: ActorRefFactory)
  : Unit = {
    val actors = recoveredActors.keyToJournalingActor.values
    val actorToKey = recoveredActors.keyToJournalingActor map { case (k, a) => a -> k }
    actorRefFactory.actorOf(
      Props {
        new Actor {
          journalActor ! JournalActor.Input.Start(recoveredActors, observer,
            recoveredJournalHeader getOrElse JournalHeader.initial(expectedJournalId getOrElse JournalId.random()))

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

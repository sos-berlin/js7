package js7.core.event.journal.recover

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import java.nio.file.{Files, Path}
import js7.base.time.ScalaTime._
import js7.base.time.{Stopwatch, Timestamp}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax._
import js7.common.event.PositionAnd
import js7.common.scalautil.Logger
import js7.common.utils.ByteUnits.toKBGB
import js7.common.utils.Exceptions.wrapException
import js7.core.event.journal.data.{JournalMeta, RecoveredJournalingActors}
import js7.core.event.journal.files.JournalFiles
import js7.core.event.journal.recover.JournalRecoverer._
import js7.core.event.journal.watch.JournalingObserver
import js7.core.event.journal.{JournalActor, KeyedJournalingActor}
import js7.data.event.{Event, EventId, JournalHeader, JournalId, JournaledState, KeyedEvent, Stamped}
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

// TODO Replace this class by JournaledStateRecoverer

/**
  * @author Joacim Zschimmer
  */
trait JournalRecoverer[S <: JournaledState[S]]
{
  protected def journalMeta: JournalMeta
  protected def expectedJournalId: Option[JournalId]
  protected def recoverSnapshot: PartialFunction[Any, Unit]
  protected def recoverEvent: PartialFunction[Stamped[KeyedEvent[Event]], Unit]
  protected def onAllSnapshotRecovered(): Unit = {}

  private val stopwatch = new Stopwatch
  private var maybeJournalHeader: Option[JournalHeader] = None
  private var _firstEventId = EventId.BeforeFirst
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
          if (_firstEventId == EventId.BeforeFirst) {
            _firstEventId = journalReader.eventId
          }
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
      logger.info(s"Recovered last EventId is ${EventId.toString(_lastEventId)}, emitted $age ago " +
        s"($snapshotCount snapshot elements and $eventCount events read in ${stopwatch.duration.pretty})")
    }
  }

  //def startJournal(journalActor: ActorRef @@ JournalActor.type, journalingObserver: Option[JournalEventWatch[E]]): Task[Completed] =
  //  Task.deferFutureAction(implicit s =>
  //    (journalActor ? JournalActor.Input.Start(RecoveredJournalingActors.Empty, journalingObserver, journalHeader))(journalMeta.askTimeout)
  //      .map { case _: JournalActor.Output.Ready => Completed })

  // TODO Use Recovered startJournalAndFinishRecoveryReplace instead
  final def startJournalAndFinishRecovery(
    journalActor: ActorRef,
    journaledState: JournaledState[S],
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty,
    journalingObserver: Option[JournalingObserver] = None)
    (implicit actorRefFactory: ActorRefFactory)
  =
    JournalRecoverer.startJournalAndFinishRecovery[S](journalActor,
      journaledState, recoveredActors,
      journalingObserver, expectedJournalId, recoveredJournalHeader,
      totalRunningSince = now - recoveredJournalHeader.fold(Duration.Zero)(_.totalRunningTime))

  final def journalId = maybeJournalHeader.map(_.journalId)

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
      totalRunningTime = maybeJournalHeader.fold(Duration.Zero) { header =>
        val lastJournalDuration = EventId.toTimestamp(_lastEventId) - EventId.toTimestamp(_firstEventId)
        header.totalRunningTime + lastJournalDuration roundUpToNext 1.ms
      },
      timestamp = lastTimestamp))

  final def lastRecoveredEventId = _lastEventId

  final def positionAndFile: Option[PositionAnd[Path]] =
    journalFileOption.map(PositionAnd(_filePosition, _))
}

object JournalRecoverer {
  private val logger = Logger(getClass)

  private[recover] def startJournalAndFinishRecovery[S <: JournaledState[S]](
    journalActor: ActorRef,
    journaledState: JournaledState[S],
    recoveredActors: RecoveredJournalingActors,
    observer: Option[JournalingObserver],
    expectedJournalId: Option[JournalId],
    recoveredJournalHeader: Option[JournalHeader],
    totalRunningSince: Deadline)
    (implicit actorRefFactory: ActorRefFactory)
  : Unit = {
    val actors = recoveredActors.keyToJournalingActor.values
    val actorToKey = recoveredActors.keyToJournalingActor map { case (k, a) => a -> k }
    actorRefFactory.actorOf(
      Props {
        new Actor {
          journalActor ! JournalActor.Input.Start(
            journaledState, recoveredActors, observer,
            recoveredJournalHeader getOrElse JournalHeader.initial(expectedJournalId getOrElse JournalId.random()),
            totalRunningSince)

          def receive = {
            case JournalActor.Output.Ready(journalHeader) =>
              for (a <- actors) {
                a ! KeyedJournalingActor.Input.FinishRecovery
              }
              logger.debug(s"Awaiting RecoveryFinished from ${actors.size} actors")
              becomeWaitingForChildren(journalHeader, actors.size)
          }

          private def becomeWaitingForChildren(journalHeader: JournalHeader, n: Int): Unit = {
            if (n == 0) {
              logger.debug(s"JournalIsReady")
              context.parent ! Output.JournalIsReady(journalHeader)
              context.stop(self)
            } else {
              context.become {
                case KeyedJournalingActor.Output.RecoveryFinished =>
                  logger.trace(s"${n - 1} actors left: Actor has RecoveryFinished: ${actorToKey(sender())}'")
                  becomeWaitingForChildren(journalHeader, n - 1)

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
    final case class JournalIsReady(journalHeader: JournalHeader)
  }
}

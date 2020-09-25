package js7.core.event.journal.recover

import akka.pattern.ask
import akka.pattern.pipe
import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import com.typesafe.config.Config
import js7.base.time.ScalaTime.DurationRichInt
import js7.common.scalautil.Logger
import js7.core.event.journal.JournalActor
import js7.core.event.journal.data.JournalMeta
import js7.core.event.journal.recover.Recovered._
import js7.core.event.journal.watch.JournalEventWatch
import js7.data.cluster.ClusterState
import js7.data.event.{EventId, JournalHeader, JournalId, JournaledState}
import scala.concurrent.duration.Deadline
import shapeless.tag.@@

final case class Recovered[S <: JournaledState[S]](
  journaledStateCompanion: JournaledState.Companion[S],
  journalMeta: JournalMeta,
  recoveredJournalFile: Option[RecoveredJournalFile[S]],
  totalRunningSince: Deadline,
  eventWatch: JournalEventWatch,
  config: Config)
extends AutoCloseable
{
  private implicit def S = journaledStateCompanion

  def close() =
    eventWatch.close()

  def eventId: EventId =
    recoveredJournalFile.fold(EventId.BeforeFirst)(_.eventId)

  def journalId: Option[JournalId] = recoveredJournalFile.map(_.journalId)

  def state: S =
    recoveredJournalFile.fold(S.empty)(_.state)

  def clusterState: ClusterState =
    state.clusterState

  def recoveredState: Option[S] =
    recoveredJournalFile.map(_.state)

  // Suppresses Config (which may contain secrets)
  override def toString = s"Recovered($journalMeta,$recoveredJournalFile,$eventWatch,Config)"

  def startJournalAndFinishRecovery(
    journalActor: ActorRef @@ JournalActor.type,
    initialJournalIdForTest: Option[JournalId] = None)
    (implicit actorRefFactory: ActorRefFactory)
  =
    actorRefFactory.actorOf(
      Props {
        new Actor {
          journalActor ! JournalActor.Input.Start(
            state, Some(eventWatch),
            recoveredJournalFile
              .map(_.calculatedJournalHeader)
              .getOrElse(JournalHeader.initial(
                recoveredJournalFile.map(_.journalId) orElse initialJournalIdForTest getOrElse JournalId.random())),
            totalRunningSince)

          def receive = {
            case JournalActor.Output.Ready(journalHeader) =>
              logger.debug(s"JournalIsReady")
              context.parent ! Output.JournalIsReady(journalHeader)
              context.stop(self)
          }
        }
      },
      name = "JournalActorRecoverer")
}

object Recovered
{
  private val logger = Logger(getClass)

  object Output {
    final case class JournalIsReady(journalHeader: JournalHeader)
  }
}

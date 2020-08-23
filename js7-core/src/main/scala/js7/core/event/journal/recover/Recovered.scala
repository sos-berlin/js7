package js7.core.event.journal.recover

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import com.typesafe.config.Config
import js7.common.scalautil.Logger
import js7.core.event.journal.data.{JournalMeta, RecoveredJournalingActors}
import js7.core.event.journal.recover.Recovered._
import js7.core.event.journal.watch.JournalEventWatch
import js7.core.event.journal.{JournalActor, KeyedJournalingActor}
import js7.data.cluster.ClusterState
import js7.data.event.{EventId, JournalHeader, JournalId, JournaledState, JournaledStateBuilder}
import scala.concurrent.duration.Deadline
import shapeless.tag.@@

final case class Recovered[S <: JournaledState[S]](
  journalMeta: JournalMeta,
  initialState: S,
  recoveredJournalFile: Option[RecoveredJournalFile[S]],
  totalRunningSince: Deadline,
  /** The recovered state */
  newStateBuilder: () => JournaledStateBuilder[S],
  eventWatch: JournalEventWatch,
  config: Config)
extends AutoCloseable
{
  def close() =
    eventWatch.close()

  def eventId: EventId =
    recoveredJournalFile.fold(EventId.BeforeFirst)(_.eventId)

  def journalId: Option[JournalId] = recoveredJournalFile.map(_.journalId)

  def state: S =
    recoveredJournalFile.fold(initialState)(_.state)

  def clusterState: ClusterState =
    state.clusterState

  def recoveredState: Option[S] =
    recoveredJournalFile.map(_.state)

  // Suppresses Config (which may contain secrets)
  override def toString = s"Recovered($journalMeta,$recoveredJournalFile,$eventWatch,Config)"

  def startJournalAndFinishRecovery(
    journalActor: ActorRef @@ JournalActor.type,
    recoveredActors: RecoveredJournalingActors = RecoveredJournalingActors.Empty,
    initialJournalIdForTest: Option[JournalId] = None)
    (implicit actorRefFactory: ActorRefFactory)
  = {
    val actors = recoveredActors.keyToJournalingActor.values
    val actorToKey = recoveredActors.keyToJournalingActor map { case (k, a) => a -> k }
    actorRefFactory.actorOf(
      Props {
        new Actor {
          journalActor ! JournalActor.Input.Start(
            state, recoveredActors, Some(eventWatch),
            recoveredJournalFile
              .map(_.calculatedJournalHeader)
              .getOrElse(JournalHeader.initial(
                recoveredJournalFile.map(_.journalId) orElse initialJournalIdForTest getOrElse JournalId.random())),
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
}

object Recovered
{
  private val logger = Logger(getClass)

  object Output {
    final case class JournalIsReady(journalHeader: JournalHeader)
  }
}

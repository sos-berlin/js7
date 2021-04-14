package js7.journal.recover

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import com.typesafe.config.Config
import js7.base.log.Logger
import js7.base.utils.SetOnce
import js7.data.cluster.ClusterState
import js7.data.event.{EventId, JournalHeader, JournalHeaders, JournalId, JournaledState}
import js7.journal.JournalActor
import js7.journal.data.JournalMeta
import js7.journal.recover.Recovered._
import js7.journal.watch.JournalEventWatch
import scala.concurrent.duration.Deadline
import shapeless.tag.@@

final class Recovered[S <: JournaledState[S]] private(
  journalMeta: JournalMeta,
  val recoveredJournalFile: Option[RecoveredJournalFile[S]],
  val totalRunningSince: Deadline,
  config: Config,
  val eventWatch: JournalEventWatch,
  journalId_ : Option[JournalId])
  (implicit S: JournaledState.Companion[S])
extends AutoCloseable
{
  private val journalIdOnce = SetOnce.fromOption(journalId_)

  def close() =
    eventWatch.close()

  /** Replace this Recovered.
    * The caller must not close the old one
    * because JournalEventWatch remains the same.
    */
  def changeRecoveredJournalFile(recoveredJournalFile: Option[RecoveredJournalFile[S]]) =
    new Recovered(journalMeta, recoveredJournalFile, totalRunningSince, config, eventWatch, journalIdOnce)

  def maybeJournalId = recoveredJournalFile.map(_.journalId)

  def eventId: EventId =
    recoveredJournalFile.fold(EventId.BeforeFirst)(_.eventId)

  def clusterState: ClusterState =
    state.clusterState

  def state: S =
    recoveredJournalFile.fold(S.empty)(_.state)

  def recoveredState: Option[S] =
    recoveredJournalFile.map(_.state)

  // Suppresses Config (which may contain secrets)
  override def toString = s"Recovered($journalMeta,$recoveredJournalFile,$eventWatch,Config)"

  def onJournalIdReplicated(journalId: JournalId): Unit =
    journalIdOnce := journalId

  def startJournalAndFinishRecovery(journalActor: ActorRef @@ JournalActor.type)
    (implicit actorRefFactory: ActorRefFactory)
  : ActorRef = {
    val journalId = journalIdOnce.getOrElse {
      logger.info("Starting a new empty journal")
      val journalId = JournalId.random()
      journalId
    }
    actorRefFactory.actorOf(
      Props {
        new Actor {
          journalActor ! JournalActor.Input.Start(
            state, Some(eventWatch),
            recoveredJournalFile
              .map(_.nextJournalHeader)
              .getOrElse(JournalHeaders.initial(journalId)),
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
}

object Recovered
{
  private val logger = Logger(getClass)

  def apply[S <: JournaledState[S]](
    journalMeta: JournalMeta,
    recoveredJournalFile: Option[RecoveredJournalFile[S]],
    totalRunningSince: Deadline,
    config: Config)
    (implicit S: JournaledState.Companion[S])
  : Recovered[S] = {
    val recoveredEventId = recoveredJournalFile.fold(EventId.BeforeFirst)(_.eventId)
    new Recovered(
      journalMeta, recoveredJournalFile, totalRunningSince, config,
      new JournalEventWatch(journalMeta, config, Some(recoveredEventId)),
      recoveredJournalFile.map(_.journalId))
  }

  object Output {
    final case class JournalIsReady(journalHeader: JournalHeader)
  }
}

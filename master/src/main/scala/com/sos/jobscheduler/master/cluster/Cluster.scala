package com.sos.jobscheduler.master.cluster

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{Uri => AkkaUri}
import akka.pattern.ask
import akka.util.Timeout
import cats.effect.Resource
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.http.{AkkaHttpClient, RecouplingStreamReader}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.Recovered
import com.sos.jobscheduler.core.event.state.JournaledStatePersistence
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, BecameSole, ClusterCoupled, FollowingStarted, SwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterState.{AwaitingAppointment, AwaitingFollower, Coupled, CoupledOrDecoupled, Decoupled, Empty, Sole}
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterNodeRole, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest}
import com.sos.jobscheduler.master.MasterState
import com.sos.jobscheduler.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import com.sos.jobscheduler.master.cluster.Cluster._
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.{Observable, OverflowStrategy}
import scala.concurrent.Promise

final class Cluster(
  journalMeta: JournalMeta,
  persistence: JournaledStatePersistence[ClusterState, ClusterEvent],
  conf: ClusterConf,
  journalActorAskTimeout: Timeout,
  actorSystem: ActorSystem)
  (implicit s: Scheduler)
{
  private val journalActor = persistence.journalActor

  // Mutable state !!!
  private val fetchingEvents = AtomicBoolean(false)
  @volatile
  private var fetchingEventsFuture: CancelableFuture[Completed] = null

  def stop(): Unit =
    for (o <- Option(fetchingEventsFuture)) {
      o.cancel()
    }

  def start(
    recovered: Recovered[MasterState, Event],
    recoveredClusterState: ClusterState,
    recoveredState: MasterState,
    getStatePromise: Promise[Task[MasterState]])
  : Task[Checked[ClusterFollowUp]] =
    startCluster(recovered, recoveredClusterState, recoveredState, getStatePromise)
      .map(_.map { case (clusterState, followUp) =>
        persistence.start(clusterState)
        followUp
      })
      .executeWithOptions(_.enableAutoCancelableRunLoops)

  private def startCluster(
    recovered: Recovered[MasterState, Event],
    recoveredClusterState: ClusterState,
    recoveredState: MasterState,
    getStatePromise: Promise[Task[MasterState]])
  : Task[Checked[(ClusterState, ClusterFollowUp)]]
  =
    (recoveredClusterState, conf.maybeOwnUri, conf.maybeRole) match {
      case (Empty, _, None | Some(_: ClusterNodeRole.Primary)) =>
        Task {
          logger.info(s"Becoming the active primary cluster node still without backup")
          Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered))
        }

      case (Empty, Some(ownUri), Some(ClusterNodeRole.Backup(activeUri))) =>
        Task { logger.info(s"Becoming the (still not following) backup cluster node '$ownUri' for primary node at $activeUri") } >>
          PassiveClusterNode.run[MasterState, Event](recovered, recoveredClusterState, recoveredState, getStatePromise,
            ownUri, activeUri, journalMeta, conf, actorSystem
          ).map(Right.apply)

      case (state, Some(ownUri), _) if state.isActive(ownUri) =>
        Task {
          state match {
            case state: Coupled => logger.info(s"Becoming the active cluster node '$ownUri' followed by the passive node ${state.passiveUri}")
            case _ => logger.info("Becoming the active cluster node without following node")
          }
          proceed(recoveredClusterState, recovered.eventId)
          Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered))
        }

      case (state: CoupledOrDecoupled, Some(ownUri), _) if state.passiveUri == ownUri =>
        Task {
          logger.info(
            if (state.isTheFollowingNode(ownUri))
              s"Becoming the following passive cluster node '$ownUri'"
            else
              s"Becoming the still not following passive cluster node '$ownUri'")
        } >>
          PassiveClusterNode.run[MasterState, Event](recovered, recoveredClusterState, recoveredState, getStatePromise,
            ownUri, state.activeUri, journalMeta, conf, actorSystem)
            .map(Right.apply)

      case (_, None, _) =>
        Task.pure(Left(Problem.pure("This cluster node's own URI is unknown")))

      case (_, Some(ownUri), _) =>
        Task.pure(Left(Problem.pure(s"This cluster node's URI '$ownUri' does not match state $recoveredClusterState")))
    }

  def automaticallyAppointConfiguredBackupNode(): Task[Checked[Completed]] =
    conf.maybeRole match {
      case Some(ClusterNodeRole.Primary(Some(backupUri))) =>
        persistence.persistTransaction(NoKey) {
          case state @ (ClusterState.Empty | _: ClusterState.Sole | _: ClusterState.AwaitingAppointment) =>
            for (events <- becomeSoleIfEmpty(state)) yield
              events ::: BackupNodeAppointed(backupUri) :: Nil
          case _ =>
            Right(Nil)
        }.map(_.toCompleted)
      case _ => Task.pure(Right(Completed))
    }

  def appointBackupNode(activeUri: Uri, backupUri: Uri): Task[Checked[Completed]] =
    conf.maybeOwnUri match {
      case None =>
        Task.pure(Left(Problem.pure("Missing this node's own URI")))
      case Some(ownUri) =>
        if (backupUri == ownUri)
          Task.pure(Left(Problem(s"A cluster node can not be appointed to itself")))
        else
          persistence.persistTransaction(NoKey) {
            case state @ (Empty | _: Sole | AwaitingAppointment(`ownUri`, `backupUri`) /*| AwaitingFollower(`ownUri`, `backupUri`)*/)
              if activeUri == ownUri =>
                for (events <- becomeSoleIfEmpty(state)) yield
                  events ::: BackupNodeAppointed(backupUri) ::
                    (state match {
                      case AwaitingAppointment(`ownUri`, `backupUri`) => ClusterCoupled :: Nil
                      case _ => Nil
                    })
            case state =>
              Left(Problem(s"A backup node can not be appointed while cluster is in state $state"))
          }.map(_.map { case (stampedEvents, state) =>
            proceed(state, stampedEvents.last.eventId)
            Completed
          })
    }

  def passiveNodesFollows(followedUri: Uri, followingUri: Uri): Task[Checked[Completed]] =
    conf.maybeOwnUri match {
      case None =>
        Task.pure(Left(Problem.pure("Missing this node's own URI")))
      case Some(ownUri) =>
        persistence.persistTransaction(NoKey) {
          case _: Coupled =>
            Right(Nil)
          case state @ (Empty | Sole(`ownUri`) | AwaitingFollower(`ownUri`, `followingUri`)) if ownUri == followedUri =>
            for (events <- becomeSoleIfEmpty(state)) yield
              events ::: FollowingStarted(followingUri) ::
                (state match {
                  case AwaitingFollower(`ownUri`, `followingUri`) => ClusterCoupled :: Nil
                  case _ => Nil
                })
          case AwaitingFollower(`ownUri`, `followingUri`) | Decoupled(`ownUri`, `followingUri`) if ownUri == followedUri =>
            Right(FollowingStarted(followingUri) :: ClusterCoupled :: Nil)
          case state =>
            Left(Problem.pure(s"Following cluster node '$followingUri' ignored due to inappropriate cluster state $state"))
        }.map(_.map { case (stampedEvents, state) =>
          for (stamped <- stampedEvents.lastOption) proceed(state, stamped.eventId)
          Completed
        })
    }

  private def becomeSoleIfEmpty(state: ClusterState): Checked[List[BecameSole]] =
    state match {
      case Empty =>
        conf.maybeOwnUri match {
          case None => Left(Problem.pure("This cluster node's own URI is unknown"))
          case Some(ownUri) => Right(BecameSole(ownUri) :: Nil)
        }
      case _ => Right(Nil)
    }

  def switchOver: Task[Checked[Completed]] =
    persistence.persistEvent[ClusterEvent](NoKey) {
      case coupled: Coupled if conf.maybeOwnUri contains coupled.activeUri =>
        Right(SwitchedOver(coupled.passiveUri))
      case state =>
        Left(Problem(s"Not switching over because Cluster is not in expected state Coupled(active=${conf.maybeOwnUri.map(_.toString)}): $state"))
    }.map(_.map(_ => Completed))

  private def proceed(state: ClusterState, eventId: EventId): Unit =
    state match {
      case state: Coupled =>
        if (!fetchingEvents.getAndSet(true)) {
          fetchingEventsFuture = fetchAndHandleAcknowledgedEventIds(state.passiveUri, after = eventId)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
            .runToFuture
        }
      case _ =>
    }

  private def fetchAndHandleAcknowledgedEventIds(uri: Uri, after: EventId): Task[Completed] =
    Task { logger.debug(s"fetchAndHandleAcknowledgedEventIds(after=$after)") } >>
      Resource.fromAutoCloseable(Task { AkkaHttpMasterApi(AkkaUri(uri.string))(actorSystem) })
        .use(api =>
          observeEventIds(api, after = after)
            .whileBusyBuffer(OverflowStrategy.DropOld(bufferSize = 2))
            .mapEval(eventId =>
              Task.deferFuture {
                (journalActor ? JournalActor.Input.FollowerAcknowledged(eventId = eventId))(journalActorAskTimeout)
                  .mapTo[Completed]
              })
            .foldL)

  private def observeEventIds(api: HttpMasterApi, after: EventId): Observable[EventId] =
    RecouplingStreamReader
      .observe[EventId, EventId, HttpMasterApi](
        zeroIndex = EventId.BeforeFirst,
        toIndex = identity,
        api,
        conf.userAndPassword,
        conf.recouplingStreamReader,
        after = after,
        getObservable = (after: EventId) =>
          AkkaHttpClient.liftProblem(
            api.eventIdObservable(
              EventRequest.singleClass[Event](after = after, timeout = Some(conf.recouplingStreamReader.timeout)))))
      .doOnError(t => Task {
        logger.debug(s"observeEventIds($api, after=$after, userAndPassword=${conf.userAndPassword}) failed with ${t.toStringWithCauses}", t)
      })
  /*
  private def queryAgents(otherNodeId: ClusterNodeId): Unit =
    Task
      .sequence(
        for (a <- votingAgentRefPaths.toVector) yield
          askAgent(a)
            .recover { case throwable =>
              logger.error(throwable.toStringWithCauses)  // We ignore the error
              Set.empty
            })
      .map(_.flatten)
      // TODO Timeout einbauen
      .runToFuture onComplete {
        case Failure(throwable) =>
          logger.error(throwable.toStringWithCauses)  // Should not happen because we have recovered already
        case Success(nodeIds) =>
          val me = nodeIds.count(_ == conf.uri)
          val other = nodeIds.count(_ == otherNodeId)
          logger.info(s"$me out of ${votingAgentRefPaths.size} Agents say this ClusterNode is reachable, $other say the other node is reachable")
          if (isAbsoluteMajority(me)) {
            self ! ClusterEvent.MajorityForMe
          } else if (isAbsoluteMajority(other)) {
            self ! ClusterEvent.MajorityFor(otherNodeId)
          }
      }

  private def isAbsoluteMajority(n: Int) =
    n > votingAgentRefPaths.size / 2
  */

  def currentState: Task[ClusterState] =
    persistence.currentState
}

object Cluster
{
  private val logger = Logger(getClass)
}

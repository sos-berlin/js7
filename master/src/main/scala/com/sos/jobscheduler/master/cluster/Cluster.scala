package com.sos.jobscheduler.master.cluster

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{Uri => AkkaUri}
import akka.pattern.ask
import akka.util.Timeout
import cats.effect.Resource
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.http.{AkkaHttpClient, RecouplingStreamReader}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.Recovered
import com.sos.jobscheduler.core.event.state.JournaledStatePersistence
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, BecameSole, ClusterCoupled, FollowingStarted, SwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterState.{AwaitingAppointment, AwaitingFollower, Coupled, CoupledOrDecoupled, Decoupled, Empty}
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterNodeId, ClusterNodeRole, ClusterState}
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

  def start(recovered: Recovered[MasterState, Event], recoveredClusterState: ClusterState, recoveredState: MasterState): Task[Checked[ClusterFollowUp]] =
    startCluster(recovered, recoveredClusterState, recoveredState)
      .map(_.map { case (clusterState, followUp) =>
        persistence.start(clusterState)
        followUp
      })
    .executeWithOptions(_.enableAutoCancelableRunLoops)

  private def startCluster(recovered: Recovered[MasterState, Event], recoveredClusterState: ClusterState, recoveredState: MasterState)
  : Task[Checked[(ClusterState, ClusterFollowUp)]]
  =
    recoveredClusterState match {
      case Empty =>
        conf.role match {
          case _: ClusterNodeRole.Primary =>
            Task.pure(Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered)))
          case ClusterNodeRole.Backup(activeUri) =>
            logger.info(s"Starting as a cluster backup node for primary node at $activeUri")
            PassiveClusterNode.run[MasterState, Event](recovered, recoveredClusterState, recoveredState, activeUri, journalMeta, conf, actorSystem)
              .map(Right.apply)
        }

      case state if state.isActive(conf.nodeId) =>
        Task.pure(Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered)))

      case state: CoupledOrDecoupled if state.passiveNodeId == conf.nodeId =>
        PassiveClusterNode.run[MasterState, Event](recovered, recoveredClusterState, recoveredState, state.activeUri, journalMeta, conf, actorSystem)
          .map(Right.apply)

      case _ =>
        Task.pure(Left(Problem.pure(s"This cluster node's ClusterNodeId does not match state $recoveredClusterState")))
    }

  def automaticallyAppointConfiguredBackupNode(): Task[Checked[Completed]] =
    conf.role match {
      case ClusterNodeRole.Primary(Some(nodeId), Some(uri)) =>
        persistence.persistTransaction(NoKey) {
          case state @ (ClusterState.Empty | _: ClusterState.Sole | _: ClusterState.AwaitingAppointment) =>
            Right(becomeSoleIfEmpty(state) ::: BackupNodeAppointed(nodeId, uri) :: Nil)
          case _ =>
            Right(Nil)
        }.map(_.toCompleted)
      case _ => Task.pure(Right(Completed))
    }

  def appointBackupNode(nodeId: ClusterNodeId, uri: Uri): Task[Checked[Completed]] =
    if (nodeId == conf.nodeId)
      Task.pure(Left(Problem(s"A cluster node can not be appointed to itself")))
    else
      persistence.persistTransaction(NoKey)(state =>
        Right(becomeSoleIfEmpty(state) :::
          BackupNodeAppointed(nodeId, uri) ::
          (state match {
            case _: AwaitingAppointment => ClusterCoupled :: Nil
            case _ => Nil
          }))
      ).map(_.map { case (stampedEvents, state) =>
        proceed(state, stampedEvents.last.eventId)
        Completed
      })

  def passiveNodesFollows(passiveNodeId: ClusterNodeId, activeUri: Uri): Task[Checked[Completed]] =
    persistence.persistTransaction(NoKey)(state =>
      Right(state match {
        case _: Coupled =>
          Nil
        case _: AwaitingFollower | _: Decoupled =>
          FollowingStarted(passiveNodeId, activeUri) :: ClusterCoupled :: Nil
        case _ =>
          becomeSoleIfEmpty(state) ::: FollowingStarted(passiveNodeId, activeUri) :: Nil
      })
    ).map(_.map { case (stampedEvents, state) =>
      for (stamped <- stampedEvents.lastOption) proceed(state, stamped.eventId)
      Completed
    })

  private def becomeSoleIfEmpty(state: ClusterState): List[BecameSole] =
    state match {
      case Empty => BecameSole(conf.nodeId) :: Nil
      case _ => Nil
    }

  def switchOver: Task[Checked[Completed]] =
    persistence.persistEvent[ClusterEvent](NoKey) {
      case coupled: Coupled if coupled.activeNodeId == conf.nodeId =>
        Right(SwitchedOver(coupled.passiveNodeId))
      case state =>
        Left(Problem(s"Not switching over because Cluster is not in expected state Coupled(active=${conf.nodeId}): $state"))
    }.map(_.map(_ => Completed))

  private def proceed(state: ClusterState, eventId: EventId): Unit =
    state match {
      case state: Coupled =>
        if (!fetchingEvents.getAndSet(true)) {
          fetchingEventsFuture = fetchAndHandleAcknowledgedEventIds(state.passiveNodeId, state.passiveUri, after = eventId)
            .runToFuture
        }
      case _ =>
    }

  private def fetchAndHandleAcknowledgedEventIds(nodeId: ClusterNodeId, uri: Uri, after: EventId): Task[Completed] =
    Task { logger.debug(s"fetchAndHandleAcknowledgedEventIds(after=$after)") } >>
      Resource.fromAutoCloseable(Task { AkkaHttpMasterApi(AkkaUri(uri.string))(actorSystem) })
        .use(api =>
          // TODO Logout and login after failure
          api.loginUntilReachable(conf.userAndPassword, Iterator.continually(1.s/*TODO*/)) >>
          observeEventIds(api, after = after, userAndPassword = None)
            .whileBusyBuffer(OverflowStrategy.DropOld(bufferSize = 2))
            .mapEval(eventId =>
              Task.deferFuture {
                (journalActor ? JournalActor.Input.FollowerAcknowledged(eventId = eventId))(journalActorAskTimeout)
                  .mapTo[Completed]
              })
            .foldL)
        .executeWithOptions(_.enableAutoCancelableRunLoops)

  private def observeEventIds(api: HttpMasterApi, after: EventId, userAndPassword: Option[UserAndPassword])(implicit s: Scheduler)
  : Observable[EventId]
  =
    RecouplingStreamReader.observe[EventId, EventId, HttpMasterApi](
      zeroIndex = EventId.BeforeFirst,
      toIndex = identity,
      api,
      userAndPassword,
      conf.recouplingStreamReader,
      after = after,
      getObservable = (after: EventId) =>
        AkkaHttpClient.liftProblem(
          api.eventIdObservable(
            EventRequest.singleClass[Event](after = after, timeout = Some(conf.recouplingStreamReader.timeout)))))

  /*
  private def queryAgents(otherNodeId: ClusterNodeId): Unit =
    Task
      .sequence(
        for (a <- votingAgentRefPaths.toVector) yield
          askAgent(a)
            .recover { case throwable =>
              logger.error(throwable.toStringWithCauses)  // We ignore the error
              Set.empty                                             /GETq
            })
      .map(_.flatten)
      // TODO Timeout einbauen
      .runToFuture onComplete {
        case Failure(throwable) =>
          logger.error(throwable.toStringWithCauses)  // Should not happen because we have recovered already
        case Success(nodeIds) =>
          val me = nodeIds.count(_ == conf.nodeId)
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

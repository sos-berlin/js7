package js7.controller.cluster

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import cats.syntax.flatMap._
import com.softwaremill.diffx
import js7.base.generic.Completed
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.base.web.{HttpClient, Uri}
import js7.common.akkahttp.https.HttpsConfig
import js7.common.event.{RealEventWatch, TornException}
import js7.common.http.RecouplingStreamReader
import js7.common.scalautil.Logger
import js7.common.system.startup.Halt.haltJava
import js7.controller.client.{AkkaHttpControllerApi, HttpControllerApi}
import js7.controller.cluster.ActivationInhibitor.inhibitActivationOfPeer
import js7.controller.cluster.ActiveClusterNode._
import js7.controller.cluster.ClusterCommon.clusterEventAndStateToString
import js7.controller.cluster.ObservablePauseDetector._
import js7.core.event.journal.JournalActor
import js7.core.event.journal.recover.Recovered
import js7.core.event.state.JournaledStatePersistence
import js7.core.problems.{ClusterCommandInapplicableProblem, ClusterNodesAlreadyAppointed, MissingPassiveClusterNodeHeartbeatProblem}
import js7.data.cluster.ClusterCommand.ClusterStartBackupNode
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterCouplingPrepared, ClusterNodesAppointed, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{ActiveShutDown, Coupled, Decoupled, Empty, HasNodes, NodesAppointed, PassiveLost, PreparedToBeCoupled}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterSetting, ClusterState, ClusterTiming}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventId, EventRequest, EventSeqTornProblem, JournaledState, KeyedEvent, Stamped}
import js7.data.node.NodeId
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import monix.execution.cancelables.SerialCancelable
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.{Observable, OverflowStrategy}
import scala.concurrent.Promise
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success}

final class ActiveClusterNode[S <: JournaledState[S]: diffx.Diff: TypeTag](
  ownId: NodeId,
  initialClusterState: ClusterState,
  httpsConfig: HttpsConfig,
  persistence: JournaledStatePersistence[S],
  eventWatch: RealEventWatch,
  common: ClusterCommon[S],
  clusterConf: ClusterConf)
  (implicit
    S: JournaledState.Companion[S],
    scheduler: Scheduler,
    actorSystem: ActorSystem,
    journalActorAskTimeout: Timeout)
{
  private val journalActor = persistence.journalActor
  private val fetchingAcks = AtomicBoolean(false)
  private val fetchingAcksCancelable = SerialCancelable()
  private val fetchingAcksTerminatedUnexpectedlyPromise = Promise[Checked[Completed]]()
  private val startingBackupNode = AtomicBoolean(false)
  @volatile
  private var switchoverAcknowledged = false
  @volatile
  private var stopRequested = false
  private val _clusterWatchSynchronizer = SetOnce[ClusterWatchSynchronizer]

  def start(recovered: Recovered[S]): Task[Completed] =
    Task.defer {
      (recovered.clusterState match {
        case clusterState: HasNodes =>
          setClusterWatchSynchronizer(clusterState.setting)
          startHeartbeating(clusterState).flatTap((_: Completed) => Task {
            logger.info("ClusterWatch agreed to restart")
          })
        case _ =>
          Task.pure(Completed)
      }).map { completed =>
        proceed(recovered.clusterState, recovered.eventId)
        completed
      }
    }

  def close(): Unit = {
    logger.debug("close")
    stopRequested = true
    for (o <- _clusterWatchSynchronizer) o.stop()
    fetchingAcksCancelable.cancel()
  }

  def beforeJournalingStarted: Task[Checked[Completed]] =
    Task.defer {
      logger.trace("beforeJournalingStarted")
      initialClusterState match {
        case clusterState: ClusterState.Coupled =>
          inhibitActivationOfPeer(clusterState.passiveUri, clusterState.timing, httpsConfig, clusterConf) map {
            case Some(otherFailedOver) =>
              Left(Problem.pure(s"While activating this node, the other node has failed-over: $otherFailedOver"))
            case None =>
              Right(Completed)
          }
        case _ =>
          Task.pure(Right(Completed))
      }
    }

  def afterJounalingStarted: Task[Checked[Completed]] =
    Task.defer {
      logger.trace("afterJounalingStarted")
      automaticallyAppointConfiguredBackupNode
        .flatMapT(_ => onRestartActiveNode)
    }

  private def automaticallyAppointConfiguredBackupNode: Task[Checked[Completed]] =
    Task.defer {
      clusterConf.maybeClusterSetting match {
        case Some(setting) =>
          assertThat(setting.activeId == ownId)
          Task(logger.trace("automaticallyAppointConfiguredBackupNode")) >>
          appointNodes(setting)
            .onErrorHandle(t => Left(Problem.fromThrowable(t)))  // We want only to log the exception
            .map {
              case Left(ClusterNodesAlreadyAppointed) =>
                Right(Completed)

              case Left(problem) =>
                Left(problem.withPrefix(
                  "Configured cluster node appointment failed, maybe due to failed access to ClusterWatch:"))

              case Right(completed) =>
                Right(completed)
            }

        case _ =>
          Task.pure(Right(Completed))
      }
    }

  def appointNodes(idToUri: Map[NodeId, Uri], activeId: NodeId, clusterWatches: Seq[ClusterSetting.Watch])
  : Task[Checked[Completed]] =
    Task.pure(ClusterSetting.checked(idToUri, activeId, clusterWatches, clusterConf.timing))
      .flatMapT(appointNodes)

  private def appointNodes(setting: ClusterSetting): Task[Checked[Completed]] =
    Task.defer {
      if (_clusterWatchSynchronizer.isEmpty) {
        setClusterWatchSynchronizer(setting)
      }
      persist() {
        case Empty =>
          Right(Some(ClusterNodesAppointed(setting)))

        case clusterState: HasNodes =>
          if (clusterState.setting != setting)
            Left(ClusterNodesAlreadyAppointed)
          else
            Right(None)
      }.map(_.map {
        case (stampedEvents, state: NodesAppointed) if stampedEvents.nonEmpty =>
          proceedNodesAppointed(state)
          Completed
        case _ =>
          Completed
      })
    }

  private def onRestartActiveNode: Task[Checked[Completed]] =
    persistence.clusterState.flatMap {
      case state: ActiveShutDown if state.activeId == ownId =>
        persist()(_ => Right(Some(ClusterActiveNodeRestarted)))
          .map(_.toCompleted)
      case _ =>
        Task.pure(Right(Completed))
    }

  def executeCommand(command: ClusterCommand): Task[Checked[ClusterCommand.Response]] =
    command match {
      case _: ClusterCommand.ClusterStartBackupNode =>
        throw new AssertionError("ClusterStartBackupNode")

      case ClusterCommand.ClusterPrepareCoupling(activeId, passiveId) =>
        requireOwnNodeId(activeId)(
          persistence.waitUntilStarted.flatMap(_ =>
            persist() {
              case Empty =>
                Left(ClusterCommandInapplicableProblem(command, Empty))

              case clusterState: HasNodes =>
                clusterState match {
                  case _ if clusterState.activeId != activeId || clusterState.passiveId != passiveId =>
                    Left(ClusterCommandInapplicableProblem(command, clusterState))

                  case _: Decoupled =>
                    Right(Some(ClusterCouplingPrepared(activeId)))

                  case _: PreparedToBeCoupled | _: Coupled =>
                    logger.debug(s"ClusterPrepareCoupling command ignored in clusterState=$clusterState")
                    Right(None)
                }
            }.map(_.map { case (stampedEvents, state) =>
              for (stamped <- stampedEvents.lastOption) proceed(state, stamped.eventId)
              ClusterCommand.Response.Accepted
            })))

      case ClusterCommand.ClusterCouple(activeId, passiveId) =>
        requireOwnNodeId(activeId)(
          persistence.waitUntilStarted >>
            persist() {
              case s: PassiveLost if s.activeId == activeId && s.passiveId == passiveId =>
                // Happens when this active node has restarted just before the passive one
                // and has already emitted a PassiveLost event.
                // We ignore this.
                // The passive node will replicate PassiveLost event and recouple
                Right(None)

              case s: PreparedToBeCoupled if s.activeId == activeId && s.passiveId == passiveId =>
                // This is the normally expected ClusterState
                Right(Some(ClusterCoupled(activeId)))

              case s: Coupled if s.activeId == activeId && s.passiveId == passiveId =>
                // Already coupled
                Right(None)

              case s =>
                Left(ClusterCommandInapplicableProblem(command, s))
            }.map(_
              .map { case (stampedEvents, state) =>
                for (stamped <- stampedEvents.lastOption) proceed(state, stamped.eventId)
                ClusterCommand.Response.Accepted
              }))

      case ClusterCommand.ClusterRecouple(activeId, passiveId) =>
        requireOwnNodeId(activeId)(
          (if (activeId != ownId)
            Task.pure(Right(Problem.pure("ClusterRecouple command may only be directed to the active node")))
          else
            persistence.waitUntilStarted >>
              persist() {
                case s: Coupled if s.activeId == activeId && s.passiveId == passiveId =>
                  // ClusterPassiveLost leads to recoupling
                  Right(Some(ClusterPassiveLost(passiveId)))

                case _ =>
                  Right(None)
              }
          ).map(_.map(_ => ClusterCommand.Response.Accepted)))

      case _: ClusterCommand.ClusterInhibitActivation =>
        throw new NotImplementedError
    }

  private def requireOwnNodeId[A](nodeId: NodeId)(body: Task[Checked[A]]): Task[Checked[A]] =
    if (nodeId != ownId)
      Task.pure(Left(Problem.pure("ClusterRecouple command may only be directed to the active node")))
    else
      body

  def switchOver: Task[Checked[Completed]] =
    stopHeartbeatingButRestartOnError {  // TODO Lock currentState.clusterState (instead in JournalStatePersistence)
      persist(suppressClusterWatch = true/*passive node will notify ClusterWatch*/) {
        case coupled: Coupled if coupled.activeId == ownId =>
          Right(Some(ClusterSwitchedOver(coupled.passiveId)))
        case state =>
          Left(Problem.pure("Switchover is possible only for the active and coupled cluster node," +
            s" but cluster state is: $state"))
      } .map(_.map { case (_: Seq[Stamped[_]], _) =>
          switchoverAcknowledged = true
          Completed
        })
    }

  def shutDownThisNode: Task[Checked[Completed]] =
    stopHeartbeatingButRestartOnError {  // TODO Lock currentState.clusterState (instead in JournalStatePersistence)
      persist() {
        case coupled: Coupled if coupled.activeId == ownId =>
          Right(Some(ClusterActiveNodeShutDown))
        case _ =>
          Right(None)
      } .map(_.map((_: (Seq[Stamped[_]], _)) => Completed))
    }

  private def proceed(state: ClusterState, eventId: EventId): Unit =
    state match {
      case state: NodesAppointed if state.activeId == ownId =>
        proceedNodesAppointed(state)

      case state: Coupled =>
        proceedCoupled(state, eventId)

      case _ =>
    }

  private def proceedNodesAppointed(state: ClusterState.NodesAppointed): Unit =
    if (state.activeId == ownId && !startingBackupNode.getAndSet(true)) {
      eventWatch.started.flatMap(_ =>
        common.tryEndlesslyToSendCommand(
          state.passiveUri,
          ClusterStartBackupNode(state.setting.copy(activeId = ownId), fileEventId = eventWatch.lastFileTornEventId)
        ) .onErrorRecover { case t: Throwable =>
            logger.warn(s"Sending Cluster command to other node failed: $t", t)
          }
      ).runToFuture
        .onComplete {
          case Success(_) =>
          case Failure(t) => logger.error(s"Appointment of cluster nodes failed: $t", t)
        }
    }

  private def proceedCoupled(state: Coupled, eventId: EventId): Unit =
    if (state.activeId == ownId) {
      fetchAndHandleAcknowledgedEventIds(state, eventId)
    } else
      sys.error(s"proceed: Unexpected ClusterState $state")

  private def fetchAndHandleAcknowledgedEventIds(initialState: Coupled, eventId: EventId): Unit =
    if (fetchingAcks.getAndSet(true)) {
      logger.debug("fetchAndHandleAcknowledgedEventIds: already fetchingAcks")
    } else {
      def msg = s"observeEventIds(${initialState.passiveUri}, after=$eventId, userAndPassword=${clusterConf.userAndPassword})"
      val future: CancelableFuture[Checked[Completed]] =
        fetchAndHandleAcknowledgedEventIds(initialState.passiveId, initialState.passiveUri, initialState.timing,
          after = eventId,
        ).flatMap {
            case Left(missingHeartbeatProblem @ MissingPassiveClusterNodeHeartbeatProblem(id)) =>
              logger.warn("No heartbeat from passive cluster node - continuing as single active cluster node")
              assertThat(id != ownId)
              // FIXME (1) Exklusiver Zugriff (Lock) wegen parallelen ClusterCommand.ClusterRecouple,
              //  das ein ClusterPassiveLost auslöst, mit ClusterCouplingPrepared infolge.
              //  Dann können wir kein ClusterPassiveLost ausgeben.
              //  JournaledStatePersistence Lock in die Anwendungsebene (hier) heben
              //  -- Nicht nötig durch die Abfrage auf initialState ?
              // FIXME (2) Deadlock when called immediately after start of Controller, before Journal has been started ?
              //  persistence.currentState may not response GetJournalState.
              persistence.clusterState/*lock ???*/.flatMap {
                case clusterState: Coupled =>
                  val passiveLost = ClusterPassiveLost(id)
                  common.ifClusterWatchAllowsActivation(clusterState, passiveLost, _clusterWatchSynchronizer.orThrow,
                    checkOnly = true,
                    Task.deferFuture {
                      // Release a concurrent persist operation, which waits for the missing acknowledgement and
                      // blocks the persist lock. Avoid a deadlock.
                      // This does not hurt if the concurrent persist operation is a ClusterEvent, too,
                      // because we are leaving ClusterState.Coupled anyway.
                      logger.debug(s"JournalActor.Input.PassiveLost($passiveLost)")
                      journalActor ? JournalActor.Input.PassiveLost(passiveLost)
                    } >>
                      stopHeartbeatingButRestartOnError(
                        persist() {
                          case `initialState` => Right(Some(passiveLost))
                          case _ => Right(None)  // Ignore when ClusterState has changed (no longer Coupled)
                        } .map(_.toCompleted.map(_ => true)))
                  ).map(_.flatMap { allowed =>
                    if (!allowed) {
                      // Should not happen
                      haltJava(s"ClusterWatch has unexpectedly forbidden activation after $passiveLost event", restart = true)
                    }
                    Left(missingHeartbeatProblem)
                  })
                case _ =>
                  Task.pure(Left(missingHeartbeatProblem))
              }
            case o => Task.pure(o)
          }
          .materialize.flatTap(tried => Task { tried match {
            case Success(Right(Completed)) =>
              if (!stopRequested) logger.error("fetchAndHandleAcknowledgedEventIds terminated unexpectedly")
            case Success(Left(_: MissingPassiveClusterNodeHeartbeatProblem)) =>
            case Success(Left(problem)) =>
              logger.error(s"$msg failed with $problem")
            case Failure(t) =>
              logger.error(s"$msg failed with ${t.toStringWithCauses}", t)
          }})
          .dematerialize
          .guarantee(Task {
            logger.debug("fetchingAcks := false")
            fetchingAcks := false
          })
          .executeWithOptions(_.enableAutoCancelableRunLoops)
          .runToFuture
        fetchingAcksCancelable := future
      future.onComplete {
        case Success(Left(_: MissingPassiveClusterNodeHeartbeatProblem)) =>
        case tried =>
          // Completes only when not cancelled and then it is a failure
          fetchingAcksTerminatedUnexpectedlyPromise.complete(tried)
      }
    }

  private def fetchAndHandleAcknowledgedEventIds(passiveId: NodeId, passiveUri: Uri, timing: ClusterTiming, after: EventId)
  : Task[Checked[Completed]] =
    Task {
      logger.info(s"Fetching acknowledgements from passive cluster node, after=${EventId.toString(after)}")
    } >>
      Observable
        .fromResource(
          AkkaHttpControllerApi.resource(passiveUri, clusterConf.userAndPassword, httpsConfig, name = "acknowledgements")(actorSystem))
        .flatMap(api =>
          observeEventIds(api, timing, after = after)
            //.map { eventId => logger.trace(s"$eventId acknowledged"); eventId }
            .whileBusyBuffer(OverflowStrategy.DropOld(bufferSize = 2))
            .detectPauses(timing.heartbeat + timing.heartbeatTimeout)
            .takeWhile(_ => !switchoverAcknowledged)  // Race condition: may be set too late
            .mapEval {
              case None/*pause*/ =>
                val problem = MissingPassiveClusterNodeHeartbeatProblem(passiveId)
                logger.trace(problem.toString)
                Task.pure(Left(problem))

              case Some(eventId) =>
                Task.deferFuture {
                  // Possible dead letter when `switchoverAcknowledged` is detected too late,
                  // because after JournalActor has committed SwitchedOver (after ack), JournalActor stops.
                  (journalActor ? JournalActor.Input.PassiveNodeAcknowledged(eventId = eventId))
                    .mapTo[Completed]
                    .map(_ => Right(eventId))
                }
            }
            //.map {
            //  case Right(eventId) => logger.trace(s"$eventId ACKNOWLEDGED"); Right(eventId)
            //  case o => logger.trace(s"Acknowledged => $o"); o
            //}
            .collect {
              case Left(problem) => problem
              //case Right(Completed) => (ignore)
            })
        .headOptionL
        .map {
          case Some(problem) => Left(problem)
          case None => Right(Completed)
        }
        .guaranteeCase(exitCase => Task(
          logger.debug(s"fetchAndHandleAcknowledgedEventIds ended => $exitCase")))

  private def observeEventIds(api: HttpControllerApi, timing: ClusterTiming, after: EventId): Observable[EventId] =
    RecouplingStreamReader
      .observe[EventId, EventId, HttpControllerApi](
        toIndex = identity,
        api,
        clusterConf.recouplingStreamReader,
        after = after,
        getObservable = (after: EventId) => {
          Task.tailRecM(after)(after2 =>
            HttpClient.liftProblem(
              api.eventIdObservable(
                EventRequest.singleClass[Event](after = after2, timeout = None),
                heartbeat = Some(timing.heartbeat))
            ) .onErrorRecover {
                case t: TornException => Left(t.problem)
              }
              .flatMap {
                case Left(torn: EventSeqTornProblem) =>
                  logger.debug(s"observeEventIds: $torn")
                  Task.pure(Left(torn.tornEventId)).delayExecution(1.s/*!!!*/)  // Repeat with last available EventId
                case o => Task.pure(Right(o))
              })
        },
        stopRequested = () => stopRequested)

  def onTerminatedUnexpectedly: Task[Checked[Completed]] =
    Task.fromFuture(fetchingAcksTerminatedUnexpectedlyPromise.future)

  private def persist(suppressClusterWatch: Boolean = false)(toEvents: ClusterState => Checked[Option[ClusterEvent]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[ClusterEvent]]], ClusterState)]] = {
    stopHeartbeating >>
    persistence.persistTransaction[ClusterEvent](NoKey)(state => toEvents(state.clusterState).map(_.toSeq))
      .flatMapT { case (stampedEvents, journaledState) =>
        val clusterState = journaledState.clusterState
        logPersisted(stampedEvents, clusterState)
        if (suppressClusterWatch || stampedEvents.isEmpty)
          Task.pure(Right(stampedEvents -> clusterState))
        else
          _clusterWatchSynchronizer.orThrow.applyEvents(stampedEvents.map(_.value.event), clusterState)
            .map(_.map((_: Completed) => stampedEvents -> clusterState))
      }
  }

  private def logPersisted(stampedEvents: Seq[Stamped[KeyedEvent[ClusterEvent]]], clusterState: ClusterState) =
    for (stamped <- stampedEvents) {
      logger.info(clusterEventAndStateToString(stamped.value.event, clusterState))
    }

  private def setClusterWatchSynchronizer(setting: ClusterSetting): ClusterWatchSynchronizer = {
    val x = common.clusterWatchSynchronizer(setting)
    _clusterWatchSynchronizer := x
    x
  }

  private def stopHeartbeatingButRestartOnError[A](task: Task[Checked[A]]): Task[Checked[A]] =
    stopHeartbeating >>
      task
        .flatMap {
          case x @ Left(_) => startHeartbeating.map(_ => x)
          case x @ Right(_) => Task.pure(x)
        }
        .onErrorHandleWith { throwable =>
          startHeartbeating
            .onErrorHandleWith(t => Task.defer {
              logger.debug("stopHeartbeatingButRestartOnError: " + throwable.toStringWithCauses)
              t.addSuppressed(throwable)
              Task.raiseError(t)
            })
            .flatMap(_ => Task.raiseError(throwable))
        }

  def startHeartbeating: Task[Completed] =
    _clusterWatchSynchronizer.orThrow.startHeartbeating

  def startHeartbeating(clusterState: ClusterState): Task[Completed] =
    _clusterWatchSynchronizer.orThrow.startHeartbeating(clusterState)

  private def stopHeartbeating: Task[Completed] =
    _clusterWatchSynchronizer.toOption.fold(Task.pure(Completed))(_.stopHeartbeating)
}

object ActiveClusterNode {
  private val logger = Logger(getClass)
}

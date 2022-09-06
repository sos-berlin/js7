package js7.cluster

import akka.pattern.ask
import akka.util.Timeout
import cats.syntax.flatMap._
import cats.syntax.monoid._
import com.softwaremill.diffx
import js7.base.generic.Completed
import js7.base.log.{CorrelId, Logger}
import js7.base.log.Logger.syntax._
import js7.base.monixutils.MonixBase.syntax._
import js7.base.monixutils.ObservablePauseDetector._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.{HttpClient, Uri}
import js7.cluster.ActiveClusterNode._
import js7.common.http.RecouplingStreamReader
import js7.common.system.startup.Halt.haltJava
import js7.data.Problems.{ClusterCommandInapplicableProblem, ClusterSettingNotUpdatable, MissingPassiveClusterNodeHeartbeatProblem}
import js7.data.cluster.ClusterCommand.ClusterStartBackupNode
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterCouplingPrepared, ClusterPassiveLost, ClusterSettingUpdated, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{ActiveShutDown, Coupled, CoupledOrDecoupled, Decoupled, Empty, HasNodes, NodesAppointed, PassiveLost, PreparedToBeCoupled}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeApi, ClusterSetting, ClusterState, ClusterTiming}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, KeyedEvent, SnapshotableState, Stamped}
import js7.data.node.NodeId
import js7.journal.JournalActor
import js7.journal.state.FileStatePersistence
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import monix.execution.cancelables.SerialCancelable
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.{Observable, OverflowStrategy}
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success}

final class ActiveClusterNode[S <: SnapshotableState[S]: diffx.Diff: TypeTag](
  initialClusterState: ClusterState.HasNodes,
  persistence: FileStatePersistence[S],
  common: ClusterCommon,
  clusterConf: ClusterConf)
  (implicit
    scheduler: Scheduler,
    journalActorAskTimeout: Timeout)
{
  private val clusterStateLock = AsyncLock("ClusterState")
  private val journalActor = persistence.journalActor
  private val fetchingAcks = AtomicBoolean(false)
  private val fetchingAcksCancelable = SerialCancelable()
  private val fetchingAcksTerminatedUnexpectedlyPromise = Promise[Checked[Completed]]()
  private val startingBackupNode = AtomicBoolean(false)
  private val sendingClusterStartBackupNode = SerialCancelable()
  @volatile private var noMoreJournaling = false
  @volatile private var stopRequested = false
  private val clusterWatchSynchronizer =
    common.initialClusterWatchSynchronizer(initialClusterState)

  import clusterConf.ownId

  def start(eventId: EventId): Task[Checked[Completed]] =
    Task.defer {
      logger.info("Asking ClusterWatch")
      assertThat(initialClusterState.activeId == ownId)
      clusterStateLock.lock(
        Task.parMap2(
          clusterWatchSynchronizer.start(initialClusterState),
          requestAcknowledgmentIfCoupled(eventId)
        )(_ |+| _)
          .flatMapT(_ => proceed(initialClusterState) map Right.apply))
    }

  private def requestAcknowledgmentIfCoupled(eventId: EventId): Task[Checked[Completed]] =
    initialClusterState match {
      case clusterState @ (_: Coupled | _: ActiveShutDown) =>
        Task(logger.info("Requesting acknowledgement for the last recovered event")) >>
          awaitAcknowledgement(clusterState.passiveUri, eventId)
            .logWhenItTakesLonger("acknowledgement")
            .flatTap {
              case Left(problem) => Task(logger.debug(problem.toString))
              case Right(Completed) => Task(logger.info("Passive node acknowledged the recovered state"))
            }
      case _ => Task.pure(Right(Completed))
    }

  def stop: Task[Completed] =
    Task.defer {
      logger.debug("stop")
      stopRequested = true
      fetchingAcksCancelable.cancel()
      clusterWatchSynchronizer.stop
    }

  def beforeJournalingStarts: Task[Checked[Completed]] =
    Task.defer {
      logger.trace("beforeJournalingStarts")
      initialClusterState match {
        case clusterState: Coupled =>
          // Inhibit activation of peer again. If recovery or asking ClusterWatch took a long time,
          // peer may have activated itself.
          common.inhibitActivationOfPeer(clusterState) map {
            case Some(otherFailedOver) =>
              Left(Problem.pure(s"While activating this node, the other node has failed-over: $otherFailedOver"))
            case None =>
              Right(Completed)
          }
        case _ =>
          Task.pure(Right(Completed))
      }
    }

  private[cluster] def appointNodes(setting: ClusterSetting): Task[Checked[Completed]] = {
    logger.debugTask(
      clusterStateLock.lock(
        suspendHeartbeat(
          persistWithoutTouchingHeartbeat() {
            case clusterState: CoupledOrDecoupled =>
              val currentSetting = clusterState.setting
              val updated = currentSetting.withPassiveUri(setting.passiveUri)
                .copy(clusterWatches = setting.clusterWatches)
              val changedPassiveUri = (setting.passiveUri != currentSetting.passiveUri) ?
                setting.passiveUri
              if (changedPassiveUri.isDefined && !clusterState.isInstanceOf[Decoupled] ||
                  updated != setting/*reject other differences*/)
                Left(ClusterSettingNotUpdatable)
              else if (updated == currentSetting)
                Right(None)
              else
                Right(Some(ClusterSettingUpdated(
                  changedPassiveUri,
                  (setting.clusterWatches != currentSetting.clusterWatches) ?
                    setting.clusterWatches)))

            case _ =>
              Left(ClusterSettingNotUpdatable)
          }.flatMapT {
            case (stampedEvents, state: HasNodes) if stampedEvents.nonEmpty =>
              proceedNodesAppointed(state) map Right.apply
            case _ =>
              Task.pure(Right(Completed))
          })))
  }

  private[cluster] def onRestartActiveNode: Task[Checked[Completed]] =
    clusterStateLock.lock(
      persist() {
        case _: ActiveShutDown =>
          Right(Some(ClusterActiveNodeRestarted))
        case _ =>
          Right(None)
      }.mapt(_ => Completed))

  def executeCommand(command: ClusterCommand): Task[Checked[ClusterCommand.Response]] =
    command match {
      case _: ClusterCommand.ClusterStartBackupNode =>
        throw new AssertionError("ClusterStartBackupNode")

      case ClusterCommand.ClusterPrepareCoupling(activeId, passiveId) =>
        requireOwnNodeId(command, activeId)(
          persistence.waitUntilStarted.flatMap(_ =>
            clusterStateLock.lock(command.toShortString)(
              persist() {
                case Empty =>
                  Left(ClusterCommandInapplicableProblem(command, Empty))

                case clusterState: HasNodes =>
                  clusterState match {
                    case _ if clusterState.activeId != activeId || clusterState.passiveId != passiveId =>
                      Left(ClusterCommandInapplicableProblem(command, clusterState))

                    case _: Decoupled =>
                      Right(Some(ClusterCouplingPrepared(activeId)))

                    case _: PreparedToBeCoupled | _: Coupled | _: ActiveShutDown =>
                      logger.debug(s"ClusterPrepareCoupling command ignored in clusterState=$clusterState")
                      Right(None)
                  }
              }.flatMapT { case (stampedEvents, clusterState) =>
                proceed(clusterState).unless(stampedEvents.isEmpty)
                  .as(Right(ClusterCommand.Response.Accepted))
              })))

      case ClusterCommand.ClusterCouple(activeId, passiveId) =>
        requireOwnNodeId(command, activeId)(
          persistence.waitUntilStarted >>
            clusterStateLock.lock(command.toShortString)(
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
              }.flatMapT { case (stampedEvents, state) =>
                proceed(state).unless(stampedEvents.isEmpty)
                  .as(Right(ClusterCommand.Response.Accepted))
              }))

      case ClusterCommand.ClusterRecouple(activeId, passiveId) =>
        requireOwnNodeId(command, activeId)(
          persistence.waitUntilStarted >>
            clusterStateLock.lock(command.toShortString)(
              persist() {
                case s: Coupled if s.activeId == activeId && s.passiveId == passiveId =>
                  // ClusterPassiveLost leads to recoupling
                  // TODO The cluster is not coupled for a short while.
                  Right(Some(ClusterPassiveLost(passiveId)))

                case _ =>
                  Right(None)
              }.map(_.map(_ => ClusterCommand.Response.Accepted))))

      case ClusterCommand.ClusterPassiveDown(activeId, passiveId) =>
        requireOwnNodeId(command, activeId)(
          clusterStateLock.lock(command.toShortString)(
            persist() {
              case s: Coupled if s.activeId == activeId && s.passiveId == passiveId =>
                Right(Some(ClusterPassiveLost(passiveId)))

              case _ =>
                Right(None)
            }.map(_.map(_ => ClusterCommand.Response.Accepted))))

      case _: ClusterCommand.ClusterInhibitActivation =>
        throw new NotImplementedError
    }

  private def requireOwnNodeId[A](command: ClusterCommand, nodeId: NodeId)(body: Task[Checked[A]]): Task[Checked[A]] =
    if (nodeId != ownId)
      Task.pure(Left(Problem.pure(s"'${command.getClass.simpleScalaName}' command may only be directed to the active node")))
    else
      body

  def switchOver: Task[Checked[Completed]] =
    clusterStateLock.lock(
      persist() {
        case coupled: Coupled =>
          Right(Some(ClusterSwitchedOver(coupled.passiveId)))
        case state =>
          Left(Problem.pure("Switchover is possible only for the active and coupled cluster node," +
            s" but cluster state is: $state"))
      } .map(_.map { case (_: Seq[Stamped[_]], _) =>
          noMoreJournaling = true
          Completed
        }))

  def shutDownThisNode: Task[Checked[Completed]] =
    clusterStateLock.lock(
      persist() {
        case _: Coupled =>
          Right(Some(ClusterActiveNodeShutDown))
        case _ =>
          Right(None)
      } .map(_.map { case (_: (Seq[Stamped[_]], _)) =>
        noMoreJournaling = true
        Completed
      }))

  private def proceed(state: ClusterState): Task[Completed] =
    state match {
      case state: NodesAppointed =>
        proceedNodesAppointed(state)

      case state: Coupled =>
        Task {
          proceedCoupled(state)
          Completed
        }

      case _ => Task.completed
    }

  private def proceedNodesAppointed(clusterState: HasNodes): Task[Completed] =
    clusterState match {
      case state: NodesAppointed =>
        Task {
          if (!startingBackupNode.getAndSet(true)) {
            startSendingClusterStartBackupNode(state)
          }
          Completed
        }

      case state: HasNodes =>
        if (clusterWatchSynchronizer.uri == state.setting.clusterWatchUri)
          Task.completed
        else
          Task.defer {
            logger.info(s"Changing ClusterWatch URI to ${state.setting.clusterWatchUri}")
            common.updateClusterWatchSynchronizer(state)
              .as(Completed)
            //, dontWait = true*
            // The new ClusterWatch must run before the ClusterAppointNodes command.
            // See also calling appointNodes and suspendHeartbeat.
          }
    }

  private def startSendingClusterStartBackupNode(clusterState: NodesAppointed): Unit = {
    val sending =
      persistence.eventWatch.started
        .flatMap(_ => common
          .tryEndlesslyToSendCommand(
            clusterState.passiveUri,
            ClusterStartBackupNode(
              clusterState.setting,
              fileEventId = persistence.eventWatch.lastFileEventId)))
        .runToFuture
    sending.onComplete {
      case Success(()) =>
      case Failure(t) => /*unexpected*/
        logger.error(s"Sending ClusterStartBackupNode command to backup node failed: $t", t)
    }
    sendingClusterStartBackupNode := sending
  }

  private def proceedCoupled(state: Coupled): Unit =
    fetchAndHandleAcknowledgedEventIds(state)

  private def fetchAndHandleAcknowledgedEventIds(initialState: Coupled): Unit =
    if (fetchingAcks.getAndSet(true)) {
      logger.debug("fetchAndHandleAcknowledgedEventIds: already fetchingAcks")
    } else {
      def msg = s"observeEventIds(${initialState.passiveUri}, peersUserAndPassword=${clusterConf.peersUserAndPassword})"
      val future: CancelableFuture[Checked[Completed]] = CorrelId.bindNew(
        fetchAndHandleAcknowledgedEventIds(initialState.passiveId, initialState.passiveUri, initialState.timing)
          .flatMap {
            case Left(missingHeartbeatProblem @ MissingPassiveClusterNodeHeartbeatProblem(id, duration)) =>
              logger.warn(s"No heartbeat from passive cluster node since ${duration.pretty} - continuing as single active cluster node")
              assertThat(id != ownId)
              // FIXME (1) Exklusiver Zugriff (Lock) wegen parallelen ClusterCommand.ClusterRecouple,
              //  das ein ClusterPassiveLost auslöst, mit ClusterCouplingPrepared infolge.
              //  Dann können wir kein ClusterPassiveLost ausgeben.
              //  StatePersistence Lock in die Anwendungsebene (hier) heben
              //  -- Nicht nötig durch die Abfrage auf initialState ?
              // FIXME (2) Deadlock when called immediately after start of Controller, before Journal has been started ?
              //  persistence.awaitCurrentState may not response GetJournalState.
              clusterStateLock.lock(
                persistence.clusterState/*lock ???*/.flatMap {
                  case clusterState: Coupled =>
                    val passiveLost = ClusterPassiveLost(id)
                    suspendHeartbeat(
                      common.ifClusterWatchAllowsActivation(clusterState, passiveLost)(
                        Task.deferFuture {
                          // Release a concurrent persist operation, which waits for the missing acknowledgement and
                          // blocks the persist lock. Avoid a deadlock.
                          // This does not hurt if the concurrent persist operation is a ClusterEvent, too,
                          // because we are leaving ClusterState.Coupled anyway.
                          logger.debug(s"JournalActor.Input.PassiveLost($passiveLost)")
                          journalActor ? JournalActor.Input.PassiveLost(passiveLost)
                        } >>
                          persistWithoutTouchingHeartbeat() {
                            case `initialState` => Right(Some(passiveLost))
                            case _ => Right(None)  // Ignore when ClusterState has changed (no longer Coupled)
                          } .map(_.toCompleted.map(_ => true)))
                    ).map(_.flatMap { allowed =>
                      if (!allowed) {
                        // Should not happen
                        haltJava(
                          "🔥 ClusterWatch has unexpectedly forbidden activation " +
                            s"after $passiveLost event",
                          restart = true)
                      }
                      Left(missingHeartbeatProblem)
                    })
                  case _ =>
                    Task.pure(Left(missingHeartbeatProblem))
                })

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
          .runToFuture)
      fetchingAcksCancelable := future
      future.onComplete {
        case Success(Left(_: MissingPassiveClusterNodeHeartbeatProblem)) =>
        case tried =>
          // Completes only when not cancelled and then it is a failure
          fetchingAcksTerminatedUnexpectedlyPromise.complete(tried)
      }
    }

  private def fetchAndHandleAcknowledgedEventIds(passiveId: NodeId, passiveUri: Uri, timing: ClusterTiming)
  : Task[Checked[Completed]] =
    Task {
      logger.info(s"Fetching acknowledgements from passive cluster node")
    } >>
      Observable
        .fromResource(
          common.clusterContext.clusterNodeApi(passiveUri, "acknowledgements"))
        .flatMap(api =>
          observeEventIds(api, Some(timing.heartbeat))
            .whileBusyBuffer(OverflowStrategy.DropOld(bufferSize = 2))
            .detectPauses(timing.passiveLostTimeout)
            .takeWhile(_ => !noMoreJournaling)  // Race condition: may be set too late
            .mapEval {
              case Left(noHeartbeatSince) =>
                val problem = MissingPassiveClusterNodeHeartbeatProblem(passiveId, noHeartbeatSince.elapsed)
                logger.trace(problem.toString)
                Task.pure(Left(problem))

              case Right(eventId) =>
                Task.deferFuture {
                  // Possible dead letter when `noMoreJournaling` is detected too late !!!
                  // because after JournalActor has committed SwitchedOver (after ack), JournalActor stops.
                  (journalActor ? JournalActor.Input.PassiveNodeAcknowledged(eventId = eventId))
                    .mapTo[Completed]
                }.map(_ => Right(eventId))
            }
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

  private def awaitAcknowledgement(passiveUri: Uri, eventId: EventId)
  : Task[Checked[Completed]] =
    Observable
      .fromResource(
        common.clusterContext.clusterNodeApi(passiveUri, "awaitAcknowledgement"))
      .flatMap(api => observeEventIds(api, heartbeat = None))
      .dropWhile(_ != eventId)
      .headOptionL
      .map {
        case Some(`eventId`) => Right(Completed)
        case _ => Left(Problem.pure(s"awaitAcknowledgement($eventId): Observable ended unexpectedly"))
      }

  private def observeEventIds(api: ClusterNodeApi, heartbeat: Option[FiniteDuration]): Observable[EventId] =
    RecouplingStreamReader
      .observe[EventId, EventId, ClusterNodeApi](
        toIndex = identity,
        api,
        clusterConf.recouplingStreamReader,
        after = -1L/*unused, web service returns always the newest EventIds*/,
        getObservable = (_: EventId) =>
          HttpClient.liftProblem(
            api.eventIdObservable(heartbeat = heartbeat)),
        stopRequested = () => stopRequested)

  def onTerminatedUnexpectedly: Task[Checked[Completed]] =
    Task.fromFuture(fetchingAcksTerminatedUnexpectedlyPromise.future)

  private def persist()(toEvents: ClusterState => Checked[Option[ClusterEvent]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[ClusterEvent]]], ClusterState)]] =
    suspendHeartbeat(
      persistWithoutTouchingHeartbeat()(toEvents))

  private def persistWithoutTouchingHeartbeat()(toEvents: ClusterState => Checked[Option[ClusterEvent]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[ClusterEvent]]], ClusterState)]] =
    Task.defer {
      assertThat(!clusterWatchSynchronizer.isHeartbeating)
      persistence.persistTransaction[ClusterEvent](NoKey) { state =>
        toEvents(state.clusterState).flatMap {
          case Some(event) if !state.clusterState.isEmptyOrActive(ownId) =>
            Left(Problem("ClusterEvent is only allowed on active cluster node: " + event))
          case events =>
            Right(events.toList)
        }
      } .flatMapT { case (stampedEvents, journaledState) =>
          assertThat(!clusterWatchSynchronizer.isHeartbeating)
          val clusterState = journaledState.clusterState
          clusterState match {
            case Empty | _: NodesAppointed =>
            case _: HasNodes => sendingClusterStartBackupNode.cancel()
          }
          val events = stampedEvents.map(_.value.event)
          (events, clusterState) match {
            case (Seq(ClusterSettingUpdated(_, Some(uris))), clusterState: HasNodes)
              if uris != clusterState.setting.clusterWatchUris =>
              // Previous ClusterWatch is probably unreachable already
              // Due to missing applyEvents the ClusterWatch must be restarted
              logger.debug("ClusterWatch applyEvents not called because ClusterWatch URI changed")
              Task.pure(Right(stampedEvents -> clusterState))

            case _ =>
              clusterWatchSynchronizer.applyEvents(events, clusterState)
                .map(_.map((_: Completed) => stampedEvents -> clusterState))
          }
        }
    }

  private def suspendHeartbeat[A](task: Task[A])
    (implicit enclosing: sourcecode.Enclosing)
  : Task[A] =
    clusterWatchSynchronizer.suspendHeartbeat(persistence.clusterState, task)
}

object ActiveClusterNode
{
  private val logger = Logger(getClass)
}

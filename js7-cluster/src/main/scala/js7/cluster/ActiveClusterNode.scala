package js7.cluster

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
import js7.base.web.{HttpClient, Uri}
import js7.cluster.ActiveClusterNode._
import js7.cluster.ClusterCommon.clusterEventAndStateToString
import js7.cluster.ObservablePauseDetector._
import js7.cluster.Problems.{ClusterCommandInapplicableProblem, ClusterSettingNotUpdatable, MissingPassiveClusterNodeHeartbeatProblem}
import js7.common.http.RecouplingStreamReader
import js7.common.scalautil.Logger
import js7.common.system.startup.Halt.haltJava
import js7.data.cluster.ClusterCommand.ClusterStartBackupNode
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterCouplingPrepared, ClusterPassiveLost, ClusterSettingUpdated, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{ActiveShutDown, Coupled, CoupledOrDecoupled, Decoupled, Empty, HasNodes, NodesAppointed, PassiveLost, PreparedToBeCoupled}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeApi, ClusterSetting, ClusterState, ClusterTiming}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournaledState, KeyedEvent, Stamped}
import js7.data.node.NodeId
import js7.journal.JournalActor
import js7.journal.state.JournaledStatePersistence
import js7.journal.watch.RealEventWatch
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import monix.execution.cancelables.SerialCancelable
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.{Observable, OverflowStrategy}
import scala.concurrent.Promise
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success}

final class ActiveClusterNode[S <: JournaledState[S]: diffx.Diff: TypeTag](
  initialClusterState: ClusterState.HasNodes,
  persistence: JournaledStatePersistence[S],
  eventWatch: RealEventWatch,
  common: ClusterCommon,
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
  private val sendingClusterStartBackupNode = SerialCancelable()
  @volatile private var switchoverAcknowledged = false
  @volatile private var stopRequested = false
  @volatile private var clusterWatchSynchronizer = common.clusterWatchSynchronizer(initialClusterState.setting)

  import clusterConf.ownId

  def start: Task[Checked[Completed]] =
    Task.defer {
      logger.info("Asking ClusterWatch")
      assertThat(initialClusterState.activeId == ownId)
      clusterWatchSynchronizer.startHeartbeating(initialClusterState)
        .map(_.map { completed =>
          logger.info("ClusterWatch agreed to restart as the active cluster node")
          proceed(initialClusterState)
          completed
        })
    }

  def close(): Unit = {
    logger.debug("close")
    stopRequested = true
    clusterWatchSynchronizer.stop()
    fetchingAcksCancelable.cancel()
  }

  def beforeJournalingStarts: Task[Checked[Completed]] =
    Task.defer {
      logger.trace("beforeJournalingStarts")
      initialClusterState match {
        case clusterState: ClusterState.Coupled =>
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

  private[cluster] def appointNodes(setting: ClusterSetting): Task[Checked[Completed]] =
    Task.defer {
      persist() {
        case clusterState: CoupledOrDecoupled =>
          val currentSetting = clusterState.setting
          val updated = currentSetting.withPassiveUri(setting.passiveUri)
            .copy(clusterWatches = setting.clusterWatches)
          val maybePassiveUri = (setting.passiveUri != currentSetting.passiveUri) ?
            setting.passiveUri
          if (maybePassiveUri.isDefined && !clusterState.isInstanceOf[Decoupled] ||
              updated != setting/*reject other differences*/)
            Left(ClusterSettingNotUpdatable)
          else if (updated == currentSetting)
            Right(None)
          else
            Right(Some(ClusterSettingUpdated(
              maybePassiveUri,
              (setting.clusterWatches != currentSetting.clusterWatches) ?
                setting.clusterWatches)))

        case _ =>
          Left(ClusterSettingNotUpdatable)
      }.map(_.map {
        case (stampedEvents, state: HasNodes) if stampedEvents.nonEmpty =>
          proceedNodesAppointed(state)
          Completed
        case _ =>
          Completed
      })
    }

  private[cluster] def onRestartActiveNode: Task[Checked[Completed]] =
    persist() {
      case _: ActiveShutDown =>
        Right(Some(ClusterActiveNodeRestarted))
      case _ =>
        Right(None)
    }.mapt(_ => Completed)

  def executeCommand(command: ClusterCommand): Task[Checked[ClusterCommand.Response]] =
    command match {
      case _: ClusterCommand.ClusterStartBackupNode =>
        throw new AssertionError("ClusterStartBackupNode")

      case ClusterCommand.ClusterPrepareCoupling(activeId, passiveId) =>
        requireOwnNodeId(command, activeId)(
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
            }.map(_.map { case (stampedEvents, clusterState) =>
              if (stampedEvents.nonEmpty) proceed(clusterState)
              ClusterCommand.Response.Accepted
            })))

      case ClusterCommand.ClusterCouple(activeId, passiveId) =>
        requireOwnNodeId(command, activeId)(
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
                if (stampedEvents.nonEmpty) proceed(state)
                ClusterCommand.Response.Accepted
              }))

      case ClusterCommand.ClusterRecouple(activeId, passiveId) =>
        requireOwnNodeId(command, activeId)(
          persistence.waitUntilStarted >>
            persist() {
              case s: Coupled if s.activeId == activeId && s.passiveId == passiveId =>
                // ClusterPassiveLost leads to recoupling
                // TODO The cluster is not coupled for a short while.
                Right(Some(ClusterPassiveLost(passiveId)))

              case _ =>
                Right(None)
            }.map(_.map(_ => ClusterCommand.Response.Accepted)))

      case _: ClusterCommand.ClusterInhibitActivation =>
        throw new NotImplementedError
    }

  private def requireOwnNodeId[A](command: ClusterCommand, nodeId: NodeId)(body: Task[Checked[A]]): Task[Checked[A]] =
    if (nodeId != ownId)
      Task.pure(Left(Problem.pure(s"'${command.getClass.simpleScalaName}' command may only be directed to the active node")))
    else
      body

  def switchOver: Task[Checked[Completed]] =
    stopHeartbeatingButRestartOnError {  // TODO Lock currentState.clusterState (instead in JournalStatePersistence)
      persist(suppressClusterWatch = true/*passive node will notify ClusterWatch*/) {
        case coupled: Coupled =>
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
        case _: Coupled =>
          Right(Some(ClusterActiveNodeShutDown))
        case _ =>
          Right(None)
      } .map(_.map((_: (Seq[Stamped[_]], _)) => Completed))
    }

  private def proceed(state: ClusterState): Unit =
    state match {
      case state: NodesAppointed =>
        proceedNodesAppointed(state)

      case state: Coupled =>
        proceedCoupled(state)

      case _ =>
    }

  private def proceedNodesAppointed(clusterState: HasNodes): Unit =
    clusterState match {
      case state: NodesAppointed =>
        if (!startingBackupNode.getAndSet(true)) {
          startSendingClusterStartBackupNode(state)
        }

      case state: HasNodes =>
        if (clusterWatchSynchronizer.clusterWatch.baseUri != state.setting.clusterWatchUri) {
          logger.info(s"Changing ClusterWatch URI to ${state.setting.clusterWatchUri}")
          clusterWatchSynchronizer.stop()
          clusterWatchSynchronizer = common.clusterWatchSynchronizer(state.setting)
        }
    }

  private def startSendingClusterStartBackupNode(clusterState: NodesAppointed): Unit = {
    val sending =
      eventWatch.started
        .flatMap(_ => common
          .tryEndlesslyToSendCommand(
            clusterState.passiveUri,
            ClusterStartBackupNode(
              clusterState.setting,
              fileEventId = eventWatch.lastFileTornEventId)))
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
      val future: CancelableFuture[Checked[Completed]] =
        fetchAndHandleAcknowledgedEventIds(initialState.passiveId, initialState.passiveUri, initialState.timing)
          .flatMap {
            case Left(missingHeartbeatProblem @ MissingPassiveClusterNodeHeartbeatProblem(id, duration)) =>
              logger.warn(s"No heartbeat from passive cluster node since ${duration.pretty} - continuing as single active cluster node")
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
                  common.ifClusterWatchAllowsActivation(clusterState, passiveLost, checkOnly = true,
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

  private def fetchAndHandleAcknowledgedEventIds(passiveId: NodeId, passiveUri: Uri, timing: ClusterTiming)
  : Task[Checked[Completed]] =
    Task {
      logger.info(s"Fetching acknowledgements from passive cluster node")
    } >>
      Observable
        .fromResource(
          common.clusterContext.clusterNodeApi(passiveUri, "acknowledgements"))
        .flatMap(api =>
          observeEventIds(api, timing)
            .whileBusyBuffer(OverflowStrategy.DropOld(bufferSize = 2))
            .detectPauses(timing.longHeartbeatTimeout)
            .takeWhile(_ => !switchoverAcknowledged)  // Race condition: may be set too late
            .mapEval {
              case Left(noHeartbeatSince) =>
                val problem = MissingPassiveClusterNodeHeartbeatProblem(passiveId, noHeartbeatSince.elapsed)
                logger.trace(problem.toString)
                Task.pure(Left(problem))

              case Right(eventId) =>
                Task.deferFuture {
                  // Possible dead letter when `switchoverAcknowledged` is detected too late,
                  // because after JournalActor has committed SwitchedOver (after ack), JournalActor stops.
                  (journalActor ? JournalActor.Input.PassiveNodeAcknowledged(eventId = eventId))
                    .mapTo[Completed]
                    .map(_ => Right(eventId))
                }
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

  private def observeEventIds(api: ClusterNodeApi, timing: ClusterTiming): Observable[EventId] =
    RecouplingStreamReader
      .observe[EventId, EventId, ClusterNodeApi](
        toIndex = identity,
        api,
        clusterConf.recouplingStreamReader,
        after = -1L/*unused, web service returns always the newest EventIds*/,
        getObservable = (_: EventId) =>
          HttpClient.liftProblem(
            api.eventIdObservable(heartbeat = Some(timing.heartbeat))),
        stopRequested = () => stopRequested)

  def onTerminatedUnexpectedly: Task[Checked[Completed]] =
    Task.fromFuture(fetchingAcksTerminatedUnexpectedlyPromise.future)

  private def persist(suppressClusterWatch: Boolean = false)(toEvents: ClusterState => Checked[Option[ClusterEvent]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[ClusterEvent]]], ClusterState)]] =
    clusterWatchSynchronizer.stopHeartbeating >>
      persistence.persistTransaction[ClusterEvent](NoKey) { state =>
        toEvents(state.clusterState).flatMap {
          case Some(event) if !state.clusterState.isEmptyOrActive(ownId) =>
            Left(Problem("ClusterEvent is only allowed on active cluster node: " + event))
          case events =>
            Right(events.toList)
        }
      } .flatMapT { case (stampedEvents, journaledState) =>
          val clusterState = journaledState.clusterState
          logPersisted(stampedEvents, clusterState)
          clusterState match {
            case Empty | _: NodesAppointed =>
            case _: HasNodes => sendingClusterStartBackupNode.cancel()
          }
          if (suppressClusterWatch || stampedEvents.isEmpty)
            Task.pure(Right(stampedEvents -> clusterState))
          else {
            val events = stampedEvents.map(_.value.event)
            (events, clusterState) match {
              case (Seq(ClusterSettingUpdated(_, Some(uris))), clusterState: HasNodes)
                if uris != clusterState.setting.clusterWatchUris =>
                logger.debug("ClusterWatch applyEvents not called because ClusterWatch URI changed")
                Task.pure(Right(stampedEvents -> clusterState))

              case _ =>
                clusterWatchSynchronizer.applyEvents(events, clusterState)
                  .map(_.map((_: Completed) => stampedEvents -> clusterState))
            }
          }
        }

  private def logPersisted(stampedEvents: Seq[Stamped[KeyedEvent[ClusterEvent]]], clusterState: ClusterState) =
    for (stamped <- stampedEvents) {
      logger.info(clusterEventAndStateToString(stamped.value.event, clusterState))
    }

  private def stopHeartbeatingButRestartOnError[A](task: Task[Checked[A]]): Task[Checked[A]] =
    clusterWatchSynchronizer.stopHeartbeating >>
      task
        .flatMap {
          case x @ Left(_) => clusterWatchSynchronizer.startHeartbeating.map(_ => x)
          case x @ Right(_) => Task.pure(x)
        }
        .onErrorHandleWith { throwable =>
          clusterWatchSynchronizer.startHeartbeating
            .onErrorHandleWith(t => Task.defer {
              logger.debug("stopHeartbeatingButRestartOnError: " + throwable.toStringWithCauses)
              t.addSuppressed(throwable)
              Task.raiseError(t)
            })
            .flatMap(_ => Task.raiseError(throwable))
        }
}

object ActiveClusterNode {
  private val logger = Logger(getClass)
}

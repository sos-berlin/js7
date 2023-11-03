package js7.cluster

import cats.syntax.flatMap.*
import cats.syntax.monoid.*
import com.softwaremill.diffx
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.monixutils.ObservablePauseDetector.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.base.utils.{AsyncLock, SetOnce}
import js7.base.web.{HttpClient, Uri}
import js7.cluster.ActiveClusterNode.*
import js7.cluster.watch.api.ClusterWatchConfirmation
import js7.common.http.RecouplingStreamReader
import js7.common.system.startup.Halt
import js7.data.Problems.{AckFromActiveClusterNodeProblem, ClusterCommandInapplicableProblem, ClusterNodeIsNotActiveProblem, ClusterSettingNotUpdatable, MissingPassiveClusterNodeHeartbeatProblem, PassiveClusterNodeUrlChangeableOnlyWhenNotCoupledProblem}
import js7.data.cluster.ClusterCommand.{ClusterConfirmCoupling, ClusterStartBackupNode}
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterCouplingPrepared, ClusterPassiveLost, ClusterSettingUpdated, ClusterSwitchedOver, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.{ActiveShutDown, Coupled, Empty, HasNodes, IsDecoupled, NodesAppointed, PassiveLost, PreparedToBeCoupled}
import js7.data.cluster.ClusterWatchProblems.{ClusterStateEmptyProblem, NoClusterWatchProblem}
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeApi, ClusterState, ClusterTiming, ClusterWatchId}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{ClusterableState, EventId, KeyedEvent, NoKeyEvent, Stamped}
import js7.data.item.BasicItemEvent.ItemAttachedToMe
import js7.data.node.NodeId
import js7.journal.JournalActor
import js7.journal.state.FileJournal
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicBoolean
import monix.execution.cancelables.SerialCancelable
import monix.reactive.{Observable, OverflowStrategy}
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import scala.concurrent.Promise
import scala.concurrent.duration.*
import scala.util.{Failure, Success}

/** Active Cluster node active which is part of a cluster (ClusterState != Empty). */
final class ActiveClusterNode[S <: ClusterableState[S]: diffx.Diff] private[cluster](
  journal: FileJournal[S],
  passiveNodeUserAndPassword: Option[UserAndPassword],
  common: ClusterCommon,
  clusterConf: ClusterConf)
  (implicit scheduler: Scheduler)
{
  private implicit val askTimeout: Timeout = common.journalActorAskTimeout
  private val clusterStateLock = AsyncLock("ClusterState")
  private val journalActor = journal.journalActor
  private val isFetchingAcks = AtomicBoolean(false)
  private val fetchingAcks = SerialCancelable()
  private val fetchingAcksTerminatedUnexpectedlyPromise = Promise[Checked[Completed]]()
  private val startingBackupNode = AtomicBoolean(false)
  private val sendingClusterStartBackupNode = SerialCancelable()
  @volatile private var noMoreJournaling = false
  @volatile private var stopRequested = false
  private val clusterWatchSynchronizerOnce = SetOnce[ClusterWatchSynchronizer]

  private def clusterWatchSynchronizer = clusterWatchSynchronizerOnce.orThrow

  import clusterConf.ownId

  def start(eventId: EventId): Task[Checked[Unit]] =
    logger.debugTask {
      // When ClusterWatchId changes at start, an ClusterWatchRegistered event is emitted,
      // changing the ClusterState (the ClusterWatchId part).
      // In this case we continue with the updated ClusterState
      def currentClusterState = journal.clusterState.map(_.asInstanceOf[HasNodes])

      currentClusterState
        .<*(clusterStateLock.lock(journal.persist { state =>
          state.clusterState match {
            case clusterState: Coupled =>
              // ClusterPassiveLost because then a ClusterWatchRegistered due to ClusterWatch change
              // does not require a passive node acknowledge which would followed by a deadlock
              // because this ActiveClusterNode is not ready yet.
              // Anyway, the PassiveClusterNode would try to send a ClusterRecouple to provoke
              // a ClusterPassiveLost. But we do it first.
              assert(clusterState.isNonEmptyActive(ownId))
              Right(Seq(
                NoKey <-: ClusterPassiveLost(clusterState.passiveId)))
            case _ => Right(Nil)
          }
        }))
        .flatMap { initialClusterState =>
          // ClusterState may have changed to PassiveLost but here we look at initialClusterState
          assertThat(initialClusterState.activeId == ownId)
          clusterStateLock.lock(Task
            .parMap2(
              // .start requires a locked clusterStateLock!
              Task.defer {
                clusterWatchSynchronizerOnce :=
                  common.initialClusterWatchSynchronizer(initialClusterState)
                clusterWatchSynchronizer.start(currentClusterState, registerClusterWatchId)
              },
              awaitAcknowledgmentIfCoupled(initialClusterState, eventId)
            )(_ |+| _)
            .flatMapT(_ => proceed(initialClusterState).as(Checked.unit)))
        }
    }

  private def awaitAcknowledgmentIfCoupled(initialClusterState: HasNodes, eventId: EventId)
  : Task[Checked[Completed]] =
    initialClusterState match {
      case clusterState @ (_: Coupled | _: ActiveShutDown) =>
        logger.info(
          s"Requesting the passive node's acknowledgement for the last recovered event ($eventId)")
        awaitAcknowledgement(clusterState.passiveUri, eventId)
          .flatMapT(ackEventId =>
            // In case of ClusterWatchRegistered, check journal.currentState.eventId too
            if (ackEventId == eventId || ackEventId == journal.unsafeCurrentState().eventId) {
              logger.info("Passive node acknowledged the recovered state")
              Task.right(Completed)
            } else
              Task.left(Problem(
                s"Passive Cluster node acknowledged $ackEventId which is not the expected EventId")))
      case _ => Task.right(Completed)
    }

  def stop: Task[Unit] =
    logger.debugTask(Task.defer {
      stopRequested = true
      fetchingAcks.cancel()
      clusterWatchSynchronizerOnce.toOption.fold(Task.unit)(_.stop)
    })

  def beforeJournalingStarts: Task[Checked[Unit]] =
    Task.defer {
      logger.trace("beforeJournalingStarts")
      journal.clusterState flatMap {
        case clusterState: Coupled =>
          // Inhibit activation of peer again. If recovery or asking ClusterWatch took a long time,
          // peer may have activated itself.
          common
            .inhibitActivationOfPeer(clusterState, passiveNodeUserAndPassword)
            .map {
              case Some(otherFailedOver) =>
                Left(Problem.pure(
                  s"While activating this node, the other node has failed-over: $otherFailedOver"))
              case None =>
                Right(Completed)
            }
        case _ =>
          Task.right(Completed)
      }
    }

  private[cluster] def changePassiveUri(
    passiveUri: Uri,
    extraEvent: Option[ItemAttachedToMe] = None)
  : Task[Checked[Unit]] =
    logger.debugTask(
      common.requireValidLicense.flatMapT(_ =>
        clusterStateLock.lock(
          suspendHeartbeat(forEvent = true)(
            persistWithoutTouchingHeartbeat() {
              case clusterState: Coupled =>
                Right(
                  (passiveUri != clusterState.setting.passiveUri) ?
                    ClusterPassiveLost(clusterState.passiveId)) // Forces recoupling
              case _ => Right(None)
            }.flatMapT(_ =>
              persistWithoutTouchingHeartbeat(extraEvent) {
                case clusterState: HasNodes =>
                  if (passiveUri == clusterState.setting.passiveUri)
                    Right(None)
                  else if (clusterState.isInstanceOf[Coupled])
                    // ClusterPassiveLost above should have avoid this
                    Left(PassiveClusterNodeUrlChangeableOnlyWhenNotCoupledProblem)
                  else
                    Right(Some(ClusterSettingUpdated(Some(passiveUri))))

                case clusterState =>
                  Left(ClusterSettingNotUpdatable(clusterState))
              }.flatMapT {
                case (stampedEvents, state: HasNodes) if stampedEvents.nonEmpty =>
                  proceedNodesAppointed(state).as(Right(()))
                case _ =>
                  Task.right(())
              })))))

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
        throw new AssertionError("ClusterStartBackupNode at active node?")

      case _: ClusterCommand.ClusterConfirmCoupling =>
        throw new AssertionError("ClusterConfirmCoupling at actice node?")

      case command @ ClusterCommand.ClusterPrepareCoupling(activeId, passiveId, _) =>
        requireOwnNodeId(command, activeId)(
          clusterStateLock.lock(command.toShortString)(
            checkCouplingToken(command).flatMapT(_ =>
              persist() {
                case Empty =>
                  Left(ClusterCommandInapplicableProblem(command, Empty))

                case clusterState: HasNodes =>
                  if (clusterState.activeId != activeId || clusterState.passiveId != passiveId)
                    Left(ClusterCommandInapplicableProblem(command, clusterState))
                  else
                    clusterState match {
                      case _: IsDecoupled =>
                        Right(Some(ClusterCouplingPrepared(activeId)))

                      case _: PreparedToBeCoupled | _: Coupled | _: ActiveShutDown =>
                        logger.debug(
                          s"ClusterPrepareCoupling command ignored in clusterState=$clusterState")
                        Right(None)
                    }
              }.flatMapT { case (stampedEvents, clusterState) =>
                proceed(clusterState).unless(stampedEvents.isEmpty)
                  .as(Right(ClusterCommand.Response.Accepted))
              })))

      case command @ ClusterCommand.ClusterCouple(activeId, passiveId, _) =>
        requireOwnNodeId(command, activeId)(
          checkCouplingToken(command).flatMapT(_ =>
            clusterStateLock.lock(command.toShortString)(
              persist() {
                case clusterState @ ClusterState.Empty =>
                  Left(ClusterCommandInapplicableProblem(command, clusterState))

                case clusterState: HasNodes =>
                  if (clusterState.activeId != activeId || clusterState.passiveId != passiveId)
                    Left(ClusterCommandInapplicableProblem(command, clusterState))
                  else
                    clusterState match {
                      case _: PassiveLost =>
                        // Happens when this active node has restarted just before the passive one
                        // and has already emitted a PassiveLost event.
                        // We ignore this.
                        // The passive node will replicate PassiveLost event and recouple
                        Right(None)

                      case s: PreparedToBeCoupled =>
                        // This is the normally expected ClusterState
                        if (!s.setting.clusterWatchId.isDefined)
                        // Passive cluster tries again until
                        // ClusterWatch has been registered via background ClusterWatch heartbeat
                          Left(NoClusterWatchProblem)
                        else
                          Right(Some(ClusterCoupled(activeId)))

                      case _: Coupled =>
                        // Already coupled
                        Right(None)

                      case s =>
                        Left(ClusterCommandInapplicableProblem(command, s))
                    }
              }.flatMapT { case (stampedEvents, state) =>
                proceed(state).unless(stampedEvents.isEmpty)
                  .as(Right(ClusterCommand.Response.Accepted))
              })))

      case ClusterCommand.ClusterRecouple(activeId, passiveId) =>
        requireOwnNodeId(command, activeId)(
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
        requireOwnNodeId(command, activeId)(Task.defer {
          logger.info(s"The passive $passiveId is shutting down")
          clusterStateLock.lock(command.toShortString)(
            persist() {
              case s: Coupled if s.activeId == activeId && s.passiveId == passiveId =>
                Right(Some(ClusterPassiveLost(passiveId)))

              case _ =>
                Right(None)
            }.map(_.map(_ => ClusterCommand.Response.Accepted)))
          })

      case _: ClusterCommand.ClusterInhibitActivation =>
        throw new NotImplementedError
    }

  private def requireOwnNodeId[A](command: ClusterCommand, nodeId: NodeId)(body: Task[Checked[A]])
  : Task[Checked[A]] =
    if (nodeId != ownId)
      Task.left(Problem.pure(
        s"'${command.getClass.simpleScalaName}' command may only be directed to the active node"))
    else
      body

  // TODO ClusterCouplingCommand soll zufällige Kennung mitgeben
  //  Neues Kommando vom Aktiven an Passiven, dass den Passiven die Kennung prüft lässt
  //  ClusterCheckCoupling(Base64UUID)
  private def checkCouplingToken(command: ClusterCommand.ClusterCouplingCommand)
  : Task[Checked[Unit]] =
    logger.debugTask(
      journal.state.map(_.clusterState)
        .map {
          case o @ ClusterState.Empty => Left(ClusterCommandInapplicableProblem(command, o): Problem)
          case HasNodes(setting) => Right(setting.passiveUri)
        }
        .flatMapT(passiveUri => common
          .clusterNodeApi(
            Admission(passiveUri, passiveNodeUserAndPassword),
            "checkCouplingToken")
          .use(api =>
            api.login(onlyIfNotLoggedIn = true) *>
              HttpClient.liftProblem(
                api.executeClusterCommand(ClusterConfirmCoupling(command.token))
                  .void))
          .timeoutTo(passiveNodeCouplingResponseTimeout, Task.left(Problem(
            s"Passive node did not respond within ${passiveNodeCouplingResponseTimeout.pretty}")))
          .onErrorHandle { throwable =>
            val msg = s"A passive cluster node wanted to couple but $passiveUri does not respond"
            logger.error(s"$msg: ${throwable.toStringWithCauses}")
            Left(Problem(msg))
          }))

  def switchOver: Task[Checked[Completed]] =
    clusterStateLock.lock(
      persist() {
        case coupled: Coupled =>
          Right(Some(ClusterSwitchedOver(coupled.passiveId)))
        case state =>
          Left(Problem.pure("Switchover is possible only for the active and coupled cluster node," +
            s" but cluster state is: $state"))
      } .map(_.map { case (_: Seq[Stamped[?]], _) =>
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
      } .map(_.map { case (_: (Seq[Stamped[?]], ?)) =>
        noMoreJournaling = true
        Completed
      }))

  private def proceed(state: ClusterState): Task[Completed] =
    state match {
      case state: NodesAppointed =>
        proceedNodesAppointed(state)

      case state: Coupled =>
        proceedCoupled(state)

      case _ => Task.completed
    }

  private def proceedNodesAppointed(clusterState: HasNodes): Task[Completed] =
    logger.traceTask(
      clusterState match {
        case clusterState: NodesAppointed =>
          Task {
            if (!startingBackupNode.getAndSet(true)) {
              startSendingClusterStartBackupNode(clusterState)
            }
            Completed
          }

        case _ =>
          Task.completed
      })

  private def startSendingClusterStartBackupNode(clusterState: NodesAppointed): Unit = {
    val sending =
      journal.eventWatch.started
        .*>(journal.state)
        .flatMap(state => common
          .tryEndlesslyToSendCommand(
            Admission(clusterState.passiveUri, passiveNodeUserAndPassword),
            ClusterStartBackupNode(
              clusterState.setting,
              fileEventId = journal.eventWatch.lastFileEventId,
              activeNodeName =
                state.clusterNodeIdToName(clusterState.activeId).orThrow,
              passiveNodeUserId =
                state.clusterNodeToUserId(clusterState.passiveId).orThrow)))
        .runToFuture
    sending.onComplete {
      case Success(()) =>
      case Failure(t) => /*unexpected*/
        logger.error(s"Sending ClusterStartBackupNode command to backup node failed: $t", t)
    }
    sendingClusterStartBackupNode := sending
  }

  private def proceedCoupled(state: Coupled): Task[Completed] =
    startFetchAndHandleAcknowledgedEventIds(state)

  private def startFetchAndHandleAcknowledgedEventIds(initialState: Coupled): Task[Completed] =
    Task {
      if (isFetchingAcks.getAndSet(true)) {
        logger.debug("fetchAndHandleAcknowledgedEventIds: already isFetchingAcks")
      } else {
        import initialState.{passiveId, passiveUri, timing}
        val future =
          CorrelId.bindNew(
            fetchAndHandleAcknowledgedEventIds(passiveId, passiveUri, timing)
          ).runToFuture
        fetchingAcks := future
        future.onComplete {
          case Success(Left(_: MissingPassiveClusterNodeHeartbeatProblem)) =>
          case tried =>
            // Completes only when not cancelled and then it is a failure
            fetchingAcksTerminatedUnexpectedlyPromise.complete(tried)
        }
      }
      Completed
    }

  private def fetchAndHandleAcknowledgedEventIds(
    passiveId: NodeId,
    passiveUri: Uri,
    timing: ClusterTiming)
  : Task[Checked[Completed]] = {
    fetchAndHandleAcknowledgedEventIds2(passiveId, passiveUri, timing)
      .flatMap {
        case Left(missingHeartbeatProblem @ MissingPassiveClusterNodeHeartbeatProblem(passiveId, duration)) =>
          logger.warn(s"❗ No heartbeat from passive cluster $passiveId since ${duration.pretty}" +
            " - trying to continue as single active cluster node")
          assertThat(passiveId != ownId)

          // FIXME (1) Exklusiver Zugriff (Lock) wegen parallelen ClusterCommand.ClusterRecouple,
          //  das ein ClusterPassiveLost auslöst, mit ClusterCouplingPrepared infolge.
          //  Dann können wir kein ClusterPassiveLost ausgeben.
          //  Journal Lock in die Anwendungsebene (hier) heben
          //  -- Nicht nötig durch die Abfrage auf initialState ?
          // FIXME (2) Deadlock when called immediately after start of Controller, before Journal has been started ?
          //  journal.awaitCurrentState may not response GetJournalState.
          /*
            ClusterPassiveLost kann während eines persist passieren, dass auf Ack des Passiven
            wartet.
            Während eines ClusterCoupled ?
            Kein lock hier, wegen möglichen Deadlocks !!!
           */
          journal.forPossibleFailoverByOtherNode(
            journal.clusterState.flatMap {
              case clusterState: Coupled =>
                val passiveLost = ClusterPassiveLost(passiveId)
                suspendHeartbeat(forEvent = true)(
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
                        case _: Coupled => Right(Some(passiveLost))
                        case _ => Right(None)  // Ignore when ClusterState has changed (no longer Coupled)
                      } .map(_.toCompleted.map(_ => true)))
                ).map(_.flatMap(allowed =>
                  if (!allowed)
                    Right(Completed)
                  else
                    Left(missingHeartbeatProblem)))
              case _ =>
                Task.pure(Left(missingHeartbeatProblem))
            })

        case o =>
          Task.pure(o)
      }
      .materialize.flatTap(tried => Task { tried match {
        case Success(Right(Completed)) =>
          if (!stopRequested) logger.error("fetchAndHandleAcknowledgedEventIds terminated unexpectedly")
        case Success(Left(_: MissingPassiveClusterNodeHeartbeatProblem)) =>
          logger.warn("❗ Continue as single active cluster node, without passive node")
        case Success(Left(problem)) =>
          logger.error(s"fetchAndHandleAcknowledgedEventIds($passiveUri) failed with $problem")

        case Failure(t: ProblemException) if t.problem is AckFromActiveClusterNodeProblem =>
          if (isTest)
            throw new RuntimeException(s"🟥 Halt suppressed for resting: ${t.problem}")
          else
            Halt.haltJava("🟥 HALT because other cluster node has become active",
              restart = true)

        case Failure(t) =>
          logger.error(
            s"fetchAndHandleAcknowledgedEventIds($passiveUri) failed with ${t.toStringWithCauses}",
            t)
      }})
      .dematerialize
      .guarantee(Task {
        logger.debug("isFetchingAcks := false")
        isFetchingAcks := false
      })
  }

  private def fetchAndHandleAcknowledgedEventIds2(passiveId: NodeId, passiveUri: Uri, timing: ClusterTiming)
  : Task[Checked[Completed]] =
    logger.debugTask(HttpClient
      .liftProblem(Observable
        .fromResource(
          common.clusterNodeApi(
            Admission(passiveUri, passiveNodeUserAndPassword),
            "acknowledgements"))
        .flatMap(api =>
          observeEventIds(api, Some(timing.heartbeat))
            .whileBusyBuffer(OverflowStrategy.DropOld(bufferSize = 2))
            .filter(_ => !clusterConf.testAckLossPropertyKey.fold(false)(k => sys.props(k).toBoolean)) // for testing
            .detectPauses(timing.passiveLostTimeout)
            .takeWhile(_ => !noMoreJournaling)  // Race condition: may be set too late
            .mapEval {
              case Left(noHeartbeatSince) =>
                val problem: Problem =
                  MissingPassiveClusterNodeHeartbeatProblem(passiveId, noHeartbeatSince.elapsed)
                logger.trace(problem.toString)
                Task.left(problem)

              case Right(eventId) =>
                Task.deferFuture {
                  // Possible dead letter when `noMoreJournaling` is detected too late !!!
                  // because after JournalActor has committed SwitchedOver (after ack), JournalActor stops.
                  (journalActor ? JournalActor.Input.PassiveNodeAcknowledged(eventId = eventId))
                    .mapTo[Completed]
                }.map(_ => Right(Completed))
            }
            .collect { case Left(problem) => problem })
        .headOptionL
        .map(_.toLeft(Completed)))
      .map(_.flatten))

  private def awaitAcknowledgement(passiveUri: Uri, eventId: EventId): Task[Checked[EventId]] =
    logger.debugTask(common
      .clusterNodeApi(Admission(passiveUri, passiveNodeUserAndPassword), "awaitAcknowledgement")
      .use(api => HttpClient
        .liftProblem(
          observeEventIds(api, heartbeat = None)
            .dropWhile(_ < eventId)
            .headOptionL
            .map(_.toRight(Problem.pure(
              s"awaitAcknowledgement($eventId): Observable ended unexpectedly"))))
        .map(_.flatten))
      .logWhenItTakesLonger("passive cluster node acknowledgement"))

  private def observeEventIds(api: ClusterNodeApi, heartbeat: Option[FiniteDuration])
  : Observable[EventId] =
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

  def executeClusterWatchConfirm(cmd: ClusterWatchConfirm): Task[Checked[Unit]] =
    common.clusterWatchCounterpart.executeClusterWatchConfirm(cmd)

  // Called back by clusterWatchCounterpart.executeClusterWatchConfirm
  private def registerClusterWatchId(confirmation: ClusterWatchConfirmation, alreadyLocked: Boolean)
  : Task[Checked[Unit]] =
    logger.traceTask(Task.defer {
      if (alreadyLocked)
        nonLockingRegisterClusterWatchId(confirmation)
      else
        clusterStateLock.lock(
          nonLockingRegisterClusterWatchId(confirmation))
    })

  private def nonLockingRegisterClusterWatchId(confirmation: ClusterWatchConfirmation)
  : Task[Checked[Unit]] =
    journal.clusterState
      .flatMap(clusterState =>
        Task(ifClusterWatchRegistered(clusterState, confirmation.clusterWatchId))
          .flatMapT(
            if (_) // Short cut
              Task.right(Nil -> clusterState)
            else
              journal
                .persistTransaction[ClusterEvent](NoKey)(s =>
                  ifClusterWatchRegistered(s.clusterState, confirmation.clusterWatchId)
                    .map(!_ thenList ClusterWatchRegistered(confirmation.clusterWatchId)))
                .map(_.map { case (stampedEvents, journaledState) =>
                  stampedEvents -> journaledState.clusterState
                })))
      .flatTapT(_ => common
        .clusterWatchCounterpart.onClusterWatchRegistered(confirmation.clusterWatchId)
        .as(Checked.unit))
      .flatMapT { case (stampedEvents, clusterState) =>
        val events = stampedEvents.map(_.value.event)
        (events, clusterState) match {
          case (Seq(event), clusterState: HasNodes) =>
            clusterWatchSynchronizer.applyEvent(event, clusterState)
              .rightAs(())

          case (Seq(), _) => Task.right(())
          case o => throw new MatchError(o)
        }
      }

  // Returns Right(true) iff clusterState settings contains clusterWatchId
  private def ifClusterWatchRegistered(clusterState: ClusterState, clusterWatchId: ClusterWatchId)
  : Checked[Boolean] =
    clusterState match {
      case ClusterState.Empty => Left(ClusterStateEmptyProblem)
      case hasNodes: HasNodes =>
        if (hasNodes.activeId != ownId)
          Left(ClusterNodeIsNotActiveProblem)
        else
          Right(hasNodes.setting.clusterWatchId contains clusterWatchId)
    }

  def onTerminatedUnexpectedly: Task[Checked[Completed]] =
    Task.fromFuture(fetchingAcksTerminatedUnexpectedlyPromise.future)

  private def persist()(toEvents: ClusterState => Checked[Option[ClusterEvent]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[ClusterEvent]]], ClusterState)]] =
    suspendHeartbeat(forEvent = true)(
      persistWithoutTouchingHeartbeat()(toEvents))

  private def persistWithoutTouchingHeartbeat(extraEvent: Option[ItemAttachedToMe] = None)(
    toEvents: ClusterState => Checked[Option[ClusterEvent]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[ClusterEvent]]], ClusterState)]] =
    Task.defer {
      assertThat(!clusterWatchSynchronizer.isHeartbeating)
      journal
        .persistTransaction[NoKeyEvent](NoKey)(state =>
          toEvents(state.clusterState).flatMap {
            case Some(event) if !state.clusterState.isEmptyOrActive(ownId) =>
              Left(Problem(s"ClusterEvent is only allowed on active cluster node: $event"))
            case maybeEvent =>
              Right((extraEvent.toList ::: maybeEvent.toList))
          })
        .flatMapT { case (stampedEvents, state) =>
          assertThat(!clusterWatchSynchronizer.isHeartbeating)
          val clusterStampedEvents = stampedEvents.collect {
            case o @ Stamped(_, _, KeyedEvent(_, _: ClusterEvent)) =>
              o.asInstanceOf[Stamped[KeyedEvent[ClusterEvent]]]
          }
          state.clusterState match {
            case Empty | _: NodesAppointed =>
            case _: HasNodes => sendingClusterStartBackupNode.cancel()
          }
          val events = clusterStampedEvents.map(_.value.event)
          (events, state.clusterState) match {
            case (Seq(event), clusterState: HasNodes) =>
              clusterWatchSynchronizer.applyEvent(event, clusterState)
                .rightAs(clusterStampedEvents -> clusterState)

            case (Seq(), clusterState) =>
              Task.right(clusterStampedEvents -> clusterState)

            case _ =>
              Task.left(Problem.pure("persistWithoutTouchingHeartbeat does not match"))
          }
        }
    }

  private def suspendHeartbeat[A](forEvent: Boolean = false)(task: Task[A])
    (implicit enclosing: sourcecode.Enclosing)
  : Task[A] =
    clusterWatchSynchronizer
      .suspendHeartbeat(journal.clusterState, forEvent = forEvent)(
        task)
}

object ActiveClusterNode
{
  private val logger = Logger[this.type]
  private val passiveNodeCouplingResponseTimeout = 3.s
}

package js7.controller.cluster

import akka.actor.ActorSystem
import akka.pattern.{AskTimeoutException, ask}
import akka.util.Timeout
import cats.data.EitherT
import cats.effect.ExitCase
import cats.syntax.flatMap._
import com.softwaremill.diffx
import com.typesafe.config.{Config, ConfigUtil}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Paths}
import js7.base.auth.UserAndPassword
import js7.base.eventbus.EventPublisher
import js7.base.generic.{Completed, SecretString}
import js7.base.monixutils.MonixBase.syntax._
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.now
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{LockResource, SetOnce}
import js7.base.web.{HttpClient, Uri}
import js7.common.akkahttp.https.HttpsConfig
import js7.common.configutils.Configs.ConvertibleConfig
import js7.common.event.{EventIdGenerator, RealEventWatch, TornException}
import js7.common.http.RecouplingStreamReader
import js7.common.scalautil.Logger
import js7.common.system.startup.Halt.haltJava
import js7.controller.client.{AkkaHttpControllerApi, HttpControllerApi}
import js7.controller.cluster.Cluster._
import js7.controller.cluster.ClusterCommon.{clusterEventAndStateToString, truncateFile}
import js7.controller.cluster.ObservablePauseDetector._
import js7.controller.data.ControllerCommand.InternalClusterCommand
import js7.core.cluster.ClusterWatch.ClusterWatchHeartbeatFromInactiveNodeProblem
import js7.core.cluster.HttpClusterWatch
import js7.core.event.journal.data.JournalMeta
import js7.core.event.journal.files.JournalFiles
import js7.core.event.journal.files.JournalFiles.JournalMetaOps
import js7.core.event.journal.recover.{JournaledStateRecoverer, Recovered}
import js7.core.event.journal.{JournalActor, JournalConf}
import js7.core.event.state.JournaledStatePersistence
import js7.core.problems.{BackupClusterNodeNotAppointed, ClusterNodeIsNotBackupProblem, ClusterNodesAlreadyAppointed, MissingPassiveClusterNodeHeartbeatProblem, PrimaryMayNotBecomeBackupProblem}
import js7.data.cluster.ClusterCommand.{ClusterInhibitActivation, ClusterStartBackupNode}
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterCouplingPrepared, ClusterNodesAppointed, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, CoupledActiveShutDown, Decoupled, Empty, FailedOver, HasNodes, NodesAppointed, PassiveLost, PreparedToBeCoupled}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterState}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventId, EventRequest, EventSeqTornProblem, JournaledState, KeyedEvent, Stamped}
import js7.data.node.NodeId
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import monix.reactive.{Observable, OverflowStrategy}
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.util.{Failure, Success}

final class Cluster[S <: JournaledState[S]: diffx.Diff](
  journalMeta: JournalMeta,
  persistence: JournaledStatePersistence[S],
  eventWatch: RealEventWatch,
  controllerId: ControllerId,
  ownId: NodeId,
  journalConf: JournalConf,
  val clusterConf: ClusterConf,
  httpsConfig: HttpsConfig,
  config: Config,
  eventIdGenerator: EventIdGenerator,
  testEventPublisher: EventPublisher[Any])
  (implicit
    S: JournaledState.Companion[S],
    journalActorAskTimeout: Timeout,
    scheduler: Scheduler,
    actorSystem: ActorSystem)
{
  private val journalActor = persistence.journalActor
  private val activationInhibitor = new ActivationInhibitor
  private val fetchingAcks = AtomicBoolean(false)
  private val fetchingAcksCancelable = SerialCancelable()
  private val fetchingAcksTerminatedUnexpectedlyPromise = Promise[Checked[Completed]]()
  private val startingBackupNode = AtomicBoolean(false)
  @volatile
  private var switchoverAcknowledged = false
  @volatile
  private var stopRequested = false
  private val started = Promise[Unit]()
  private var activated = false
  @volatile
  private var _currentClusterState: Task[ClusterState] = null
  private var remainingActiveAfterRestart = false
  private val expectingStartBackupCommand = SetOnce[Promise[ClusterStartBackupNode]]

  private lazy val clusterWatch = {
    val uri = clusterConf.agentUris.headOption
      .getOrElse(sys.error("Missing ClusterWatch / Agent URI in cluster configuration"))
    new HttpClusterWatch(
      uri,
      userAndPassword = config.optionAs[SecretString]("js7.auth.agents." + ConfigUtil.joinPath(uri.string))
        .map(password => UserAndPassword(controllerId.toUserId, password)),
      httpsConfig = httpsConfig,
      actorSystem)
  }

  def stop(): Unit = {
    logger.debug("stop")
    stopRequested = true
    fetchingAcksCancelable.cancel()
    clusterWatchSynchronizer.stop()
    //TODO clusterWatch.logout()
  }

  /**
    * Returns a pair of `Task`s
    * - `(Task[None], _)` no replicated state available, because it's an active node or the backup has not yet appointed.
    * - `(Task[Some[Checked[S]]], _)` with the current replicated state if this node has started as a passive node.
    * - `(_, Task[Checked[ClusterFollowUp]]` when this node should be activated
    * @return A pair of `Task`s with maybe the current `S` of this passive node (if so)
    *         and ClusterFollowUp.
    */
  def start(recovered: Recovered[S], recoveredState: S): (Task[Option[Checked[S]]], Task[Checked[ClusterFollowUp[S]]]) = {
    if (recovered.clusterState != Empty) logger.info(s"Recovered ClusterState is ${recovered.clusterState}")

    val (replicatedState, followUp) = startCluster(recovered, recoveredState)

    _currentClusterState = replicatedState.map(_.fold(recovered.clusterState)(_.map(_.clusterState).orThrow/*???*/))
    started.success(())
    replicatedState ->
      followUp.map(_.map { case (clusterState, followUp) =>
        activated = followUp.isInstanceOf[ClusterFollowUp.BecomeActive[_]]
        clusterWatchSynchronizer.scheduleHeartbeats(clusterState)
        followUp
      })
  }

  private def startCluster(recovered: Recovered[S], recoveredState: S)
  : (Task[Option[Checked[S]]], Task[Checked[(ClusterState, ClusterFollowUp[S])]])
  =
    (recovered.clusterState, recovered.recoveredJournalFile) match {
      case (Empty, _) =>
        if (!clusterConf.isBackup) {
          logger.debug(s"Active primary cluster node '$ownId', still with no backup node appointed")
          Task.pure(None) ->
            (activationInhibitor.startActive >>
              Task.pure(Right(recovered.clusterState -> ClusterFollowUp.BecomeActive(recovered))))
        } else if (recovered.eventId != EventId.BeforeFirst)
          Task.pure(Some(Left(PrimaryMayNotBecomeBackupProblem))) -> Task.pure(Left(PrimaryMayNotBecomeBackupProblem))
        else {
          logger.info(s"Backup cluster node '$ownId', awaiting appointment from a primary node")
          val startedPromise = Promise[ClusterStartBackupNode]()
          expectingStartBackupCommand := startedPromise
          val passiveClusterNode = Task.deferFuture(startedPromise.future)
            .map(cmd => newPassiveClusterNode(cmd.idToUri, cmd.activeId, recovered,
              initialFileEventId = Some(cmd.fileEventId)))
            .memoize
          Task.defer(
            if (startedPromise.future.isCompleted)
              passiveClusterNode.flatMap(_.state.map(s => Some(Right(s))))
            else
              Task.pure(Some(Left(BackupClusterNodeNotAppointed)))
          ) ->
            passiveClusterNode.flatMap(passive =>
              activationInhibitor.startPassive >>
                passive.run(recoveredState))
        }

      case (recoveredClusterState: HasNodes, Some(recoveredJournalFile)) if recoveredClusterState.activeId == ownId =>
        assertThat(recoveredClusterState == recoveredJournalFile.state.clusterState)
        recoveredClusterState match {
          case recoveredClusterState: Coupled =>
            import recoveredClusterState.{passiveId, passiveUri}
            logger.info(s"This cluster node '$ownId' was coupled before restart - asking '$passiveId' about its state")
            val failedOver = inhibitActivationOf(passiveUri).map {
              case None =>
                // The other node has not failed-over
                logger.info(s"The other cluster node '$passiveId' is still passive, so this node remains the active cluster node")
                remainingActiveAfterRestart = true
                proceedCoupled(recoveredClusterState, recovered.eventId)
                None

              case Some(otherFailedOver) =>
                import otherFailedOver.failedAt
                logger.warn(s"The other cluster node '${otherFailedOver.activeId}' has become active (failed-over) while this node was absent")
                assertThat(otherFailedOver.idToUri == recoveredClusterState.idToUri &&
                           otherFailedOver.activeId == recoveredClusterState.passiveId)
                // This restarted, previously failed cluster node may have written one chunk of events more than
                // the passive node, maybe even an extra snapshot in a new journal file.
                // These extra events are not acknowledged. So we truncate the journal.
                var truncated = false
                val journalFiles = JournalFiles.listJournalFiles(journalMeta.fileBase) takeRight 2
                val journalFile =
                  if (journalFiles.last.afterEventId == failedAt.fileEventId)
                    journalFiles.last
                  else if (journalFiles.lengthIs == 2 &&
                    journalFiles.head.afterEventId == failedAt.fileEventId &&
                    journalFiles.last.afterEventId > failedAt.fileEventId)
                  {
                    logger.info(s"Removing journal file written after failover: ${recoveredJournalFile.file.getFileName}")
                    truncated = true
                    val deleteFile = journalFiles.last.file
                    Files.move(deleteFile, Paths.get(s"$deleteFile~DELETED-AFTER-FAILOVER"), REPLACE_EXISTING)  // Keep the file for debugging
                    journalMeta.updateSymbolicLink(journalFiles.head.file)
                    journalFiles.head
                  } else
                    sys.error(s"Failed-over node's ClusterState does not match local journal files:" +
                      s" $otherFailedOver <-> ${journalFiles.map(_.file.getFileName).mkString(", ")}")
                assertThat(journalFile.afterEventId == failedAt.fileEventId)
                //FIXME JournalEventWatch darf noch nicht über den abgeschnittenen Teil benachrichtigt worden sein!
                //assertThat(recoveredJournalFile.journalPosition == failedAt,
                //  s"${recoveredJournalFile.journalPosition} != $failedAt")
                val file = journalFile.file
                val fileSize = Files.size(file)
                if (fileSize != failedAt.position) {
                  if (fileSize < failedAt.position)
                    sys.error(s"Journal file '${journalFile.file.getFileName} is shorter than the failed-over position ${failedAt.position}")
                  logger.info(s"Truncating journal file at failover position ${failedAt.position} (${fileSize - failedAt.position} bytes): ${journalFile.file.getFileName}")
                  truncated = true
                  truncateFile(file, failedAt.position)
                }
                var trunkRecovered = recovered
                if (truncated) {
                  // TODO Recovering may be omitted because the new active node has written a snapshot immediately after failover
                  // May take a long time !!!
                  logger.info("Recovering again from properly truncated journal file")
                  trunkRecovered = JournaledStateRecoverer.recover[S](journalMeta, config /*, runningSince=???*/)
                  val truncatedRecoveredJournalFile = trunkRecovered.recoveredJournalFile
                    .getOrElse(sys.error(s"Unrecoverable journal file '${file.getFileName}''"))
                  assertThat(truncatedRecoveredJournalFile.state.clusterState == recoveredClusterState)
                  assertThat(truncatedRecoveredJournalFile.journalPosition == failedAt,
                    s"${truncatedRecoveredJournalFile.journalPosition} != $failedAt")
                  //TODO Missing test: recovered.close()
                }
                Some(recoveredClusterState ->
                  newPassiveClusterNode(otherFailedOver.idToUri, otherFailedOver.activeId, trunkRecovered,
                   otherFailedOver = true))

              case otherState =>
                sys.error(s"The other node is in invalid failedOver=$otherState")  // ???
            }.memoize

            failedOver.flatMap {
              case None => Task.pure(None)
              case Some((_, passiveClusterNode)) => passiveClusterNode.state.map(s => Some(Right(s)))
            } ->
              failedOver.flatMap {
                case None =>
                  Task.pure(Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered)))
                case Some((otherClusterState/*unused???*/, passiveClusterNode)) =>
                  assertThat(otherClusterState == recoveredState.clusterState)
                  passiveClusterNode.run(recoveredState)
              }

          case _ =>
            logger.info("Remaining the active cluster node without following node")
            proceed(recoveredClusterState, recovered.eventId)
            Task.pure(None) ->
              (activationInhibitor.startActive >>
                Task.pure(Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered))))
        }

      case (state: HasNodes, Some(_)) if state.passiveId == ownId =>
        logger.info(
          if (state.isInstanceOf[Coupled])
            s"Remaining a passive cluster node following the active node '${state.activeId}'"
          else
            s"Remaining a passive cluster node trying to follow the active node '${state.activeId}'")
        val passive = newPassiveClusterNode(state.idToUri, state.activeId, recovered)
        passive.state.map(s => Some(Right(s))) ->
          passive.run(recoveredState)

      case _ =>
        Task.pure(None) ->
          Task.pure(Left(Problem.pure(s"Unexpected clusterState=${recovered.clusterState} (id=$ownId)")))
    }

  private def newPassiveClusterNode(
    idToUri: Map[NodeId, Uri],
    activeId: NodeId,
    recovered: Recovered[S],
    otherFailedOver: Boolean = false,
    initialFileEventId: Option[EventId] = None)
  : PassiveClusterNode[S]
  = {
    val common = new ClusterCommon(activationInhibitor, clusterWatch, ownId, clusterConf, httpsConfig, testEventPublisher)
    new PassiveClusterNode(ownId, idToUri, activeId, journalMeta, initialFileEventId, recovered,
      otherFailedOver, journalConf, clusterConf, eventIdGenerator, common)
  }

  private def automaticallyAppointConfiguredBackupNode: Task[Completed] =
    Task.defer {
      clusterConf.maybeIdToUri match {
        case Some(idToUri) =>
          Task(logger.trace("automaticallyAppointConfiguredBackupNode")) >>
          appointNodes(idToUri, ownId)
            .onErrorHandle(t => Left(Problem.fromThrowable(t)))  // We want only to log the exception
            .map {
              case Left(ClusterNodesAlreadyAppointed) => Completed
              case Left(problem) =>
                // If we return Left(problem), Agent would fail. Better we return Completed.
                // JS7 Suite does not use configured appointment, anyway.
                // Should we notify someone ???
                logger.error(s"Configured cluster node appointment failed due to failed access to ClusterWatch: $problem",
                  problem.throwableOption.orNull)
                Completed
              case Right(completed) => completed
            }
        case _ => Task.pure(Completed)
      }
    }

  def appointNodes(idToUri: Map[NodeId, Uri], activeId: NodeId): Task[Checked[Completed]] =
    persist {
      case Empty =>
        ClusterNodesAppointed.checked(idToUri, activeId).map(Some.apply)
      case NodesAppointed(`idToUri`, `activeId`) =>
        Right(None)
      case clusterState: HasNodes =>
        if (clusterState.idToUri == idToUri)
          Right(None)
        else
          Left(ClusterNodesAlreadyAppointed)
    }.map(_.map {
      case (stampedEvents, state: NodesAppointed) if stampedEvents.nonEmpty =>
        proceedNodesAppointed(state)
        Completed
      case _ =>
        Completed
    })

  def executeCommand(command: ClusterCommand): Task[Checked[ClusterCommand.Response]] =
    command match {
      case command: ClusterCommand.ClusterStartBackupNode =>
        Task {
          expectingStartBackupCommand.toOption match {
            case None =>
              if (!clusterConf.isBackup)
                Left(ClusterNodeIsNotBackupProblem)
              else
                Left(Problem("Cluster node is not ready to accept a backup node configuration"))
            case Some(promise) =>
              if (command.passiveId != ownId)
                Left(Problem.pure(s"$command sent to wrong node '$ownId'"))
              else if (command.activeId == ownId)
                Left(Problem.pure(s"$command must not be sent to the active node"))
              else {
                promise.trySuccess(command)
                Right(ClusterCommand.Response.Accepted)
              }
          }
        }

      case ClusterCommand.ClusterPrepareCoupling(activeId, passiveId) =>
        persistence.waitUntilStarted.flatMap(_ =>
          persist {
            case clusterState: Decoupled
              if clusterState.activeId == activeId && clusterState.passiveId == passiveId =>
              Right(Some(ClusterCouplingPrepared(activeId)))

            case clusterState: PreparedToBeCoupled
              if clusterState.activeId == activeId  && clusterState.passiveId == passiveId =>
              logger.debug(s"PreparedToBeCoupled ignored in clusterState=$clusterState")
              Right(None)

            case clusterState: Coupled
              if clusterState.activeId == activeId && clusterState.passiveId == passiveId =>
              logger.debug(s"ClusterPrepareCoupling ignored in clusterState=$clusterState")
              Right(None)

            case state =>
              Left(Problem.pure(s"$command command rejected due to inappropriate cluster state $state"))
          }.map(_.map { case (stampedEvents, state) =>
            for (stamped <- stampedEvents.lastOption) proceed(state, stamped.eventId)
            ClusterCommand.Response.Accepted
          }))

      case ClusterCommand.ClusterCouple(activeId, passiveId) =>
        persistence.waitUntilStarted >>
          persist {
            case s: PassiveLost if s.activeId == activeId && s.passiveId == passiveId =>
              // Happens when this active node has restarted just before the passive one
              // and has already emitted a PassiveLost event
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
              Left(Problem.pure(s"$command command rejected due to inappropriate cluster state $s"))
          }.map(_
            .map { case (stampedEvents, state) =>
              for (stamped <- stampedEvents.lastOption) proceed(state, stamped.eventId)
              ClusterCommand.Response.Accepted
            })

      case ClusterCommand.ClusterRecouple(activeId, passiveId) =>
        (if (activeId != ownId)
          Task.pure(Right(Problem.pure("ClusterRecouple command may only be directed to the active node")))
        else
          persistence.waitUntilStarted >>
            persist {
              case s: Coupled if s.activeId == activeId && s.passiveId == passiveId =>
                Right(Some(ClusterPassiveLost(passiveId)))

              case _ =>
                Right(None)
            }
        ).map(_.map(_ => ClusterCommand.Response.Accepted))

      case ClusterCommand.ClusterInhibitActivation(duration) =>
        import ClusterCommand.ClusterInhibitActivation.Response
        activationInhibitor.inhibitActivation(duration)
          .flatMapT {
            case /*inhibited=*/true => Task.pure(Right(Response(None)))
            case /*inhibited=*/false =>
              // Could not inhibit, so this node is already active
              persistence.currentState.map(_.clusterState).map {
                case failedOver: FailedOver =>
                  logger.debug(s"inhibitActivation(${duration.pretty}) => $failedOver")
                  Right(Response(Some(failedOver)))
                case clusterState =>
                  Left(Problem.pure(
                    s"ClusterInhibitActivation command failed because node is already active but not failed-over: $clusterState"))
              }
          }
    }

  def beforeJournalingStarted: Task[Checked[Completed]] =
    Task.defer {
      logger.trace("beforeJournalingStarted")
      if (remainingActiveAfterRestart)
        inhibitActivationOfPeer.map {
          case Some(otherFailedOver) =>
            Left(Problem.pure(s"While activating this node, the other node has failed-over: $otherFailedOver"))
          case None =>
            Right(Completed)
        }
      else
        Task.pure(Right(Completed))
    }

  def afterJounalingStarted: Task[Checked[Completed]] =
    Task.defer {
      logger.trace("afterJounalingStarted")
      _currentClusterState = persistence.currentState.map(_.clusterState)
      automaticallyAppointConfiguredBackupNode >>
        onRestartActiveNode
    }

  private def onRestartActiveNode: Task[Checked[Completed]] =
    persistence.currentState.map(_.clusterState).flatMap {
      case state: CoupledActiveShutDown if state.activeId == ownId =>
        persist(_ => Right(Some(ClusterActiveNodeRestarted)))
          .map(_.toCompleted)
      case _ =>
        Task.pure(Right(Completed))
    }

  private def inhibitActivationOfPeer: Task[Option[FailedOver]] =
    _currentClusterState.flatMap {
      case clusterState: ClusterState.HasNodes if clusterState.activeId == ownId =>
        inhibitActivationOf(clusterState.passiveUri)

      case clusterState => Task.raiseError(new IllegalStateException(
        s"inhibitActivationOfPeer expects active cluster node, but clusterState=$clusterState"))
    }

  private def inhibitActivationOf(uri: Uri): Task[Option[FailedOver]] =
    Task.defer {
      val retryDelay = 5.s  // TODO
      AkkaHttpControllerApi.resource(uri, clusterConf.userAndPassword, httpsConfig, name = "ClusterInhibitActivation")
        .use(otherNode =>
          otherNode.loginUntilReachable() >>
            otherNode.executeCommand(
              InternalClusterCommand(
                ClusterInhibitActivation(2 * clusterConf.failAfter/*TODO*/))
            ) .map(_.response.asInstanceOf[ClusterInhibitActivation.Response].failedOver))
        .onErrorRestartLoop(()) { (throwable, _, retry) =>
          // TODO Code mit loginUntilReachable usw. zusammenfassen. Stacktrace unterdrücken wenn isNotIgnorableStackTrace
          val msg = "While trying to reach the other cluster node due to restart: " + throwable.toStringWithCauses
          logger.warn(msg)
          for (t <- throwable.ifNoStackTrace) logger.debug(msg, t)
          retry(()).delayExecution(retryDelay)
        }
    }.map { maybeFailedOver =>
      logger.debug(s"$uri ClusterInhibitActivation returned failedOver=$maybeFailedOver")
      maybeFailedOver
    }

  def switchOver: Task[Checked[Completed]] =
    clusterWatchSynchronizer.stopHeartbeats() >>
      persist {
        case coupled: Coupled if coupled.activeId == ownId =>
          Right(Some(ClusterSwitchedOver(coupled.passiveId)))
        case state =>
          Left(Problem.pure(s"Switchover is possible only for the active cluster node," +
            s" but cluster state is: $state"))
      } .map(_.map { case (_: Seq[Stamped[_]], _) =>
          switchoverAcknowledged = true
          Completed
        })
        .guaranteeCase {
          case ExitCase.Error(_) =>
            persistence.currentState.map(_.clusterState).flatMap(clusterState =>  // TODO Lock currentClusterState (instead in JournalStatePersistence)
              Task { clusterWatchSynchronizer.scheduleHeartbeats(clusterState) })
          case _ => Task.unit
        }

  def shutDownThisNode: Task[Checked[Completed]] =
    clusterWatchSynchronizer.stopHeartbeats() >>
      persist {
        case coupled: Coupled if coupled.activeId == ownId =>
          Right(Some(ClusterActiveNodeShutDown))
        case _ =>
          Right(None)
      } .map(_.map((_: (Seq[Stamped[_]], _)) => Completed))
        .guaranteeCase {
          case ExitCase.Error(_) =>
            persistence.currentState.map(_.clusterState).flatMap(clusterState =>  // TODO Lock currentClusterState (instead in JournalStatePersistence)
              Task { clusterWatchSynchronizer.scheduleHeartbeats(clusterState) })
          case _ => Task.unit
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
        val common = new ClusterCommon(activationInhibitor, clusterWatch, ownId, clusterConf, httpsConfig, testEventPublisher)
        eventWatch.started.flatMap(_ =>
          common.tryEndlesslyToSendCommand(
            state.passiveUri,
            ClusterStartBackupNode(state.idToUri, activeId = ownId, fileEventId = eventWatch.lastFileTornEventId)
          ) .onErrorRecover { case t: Throwable =>
              logger.warn(s"Sending Cluster command to other node failed: $t", t)
            }
        ).runToFuture
          .onComplete {
            case Success(_) =>
            case Failure(t) => logger.error(s"Appointment of cluster nodes failed: $t", t)
          }
      }

    private def proceedCoupled(state: Coupled, eventId: EventId): Unit = {
      if (state.activeId == ownId) {
        fetchAndHandleAcknowledgedEventIds(state, eventId)
      } else
        sys.error(s"proceed: Unexpected ClusterState $state")
    }

  private def fetchAndHandleAcknowledgedEventIds(initialState: Coupled, eventId: EventId): Unit =
    if (fetchingAcks.getAndSet(true)) {
      logger.debug("fetchAndHandleAcknowledgedEventIds: already fetchingAcks")
    } else {
      def msg = s"observeEventIds(${initialState.passiveUri}, after=$eventId, userAndPassword=${clusterConf.userAndPassword})"
      val future: CancelableFuture[Checked[Completed]] =
        fetchAndHandleAcknowledgedEventIds(initialState.passiveId, initialState.passiveUri, after = eventId)
          .flatMap {
            case Left(missingHeartbeatProblem @ MissingPassiveClusterNodeHeartbeatProblem(id)) =>
              logger.warn("No heartbeat from passive cluster node - continuing as single active cluster node")
              assertThat(id != ownId)
              val passiveLost = ClusterPassiveLost(id)
              val common = new ClusterCommon(activationInhibitor, clusterWatch, ownId, clusterConf, httpsConfig, testEventPublisher)
              // FIXME (1) Exklusiver Zugriff (Lock) wegen parallelen ClusterCommand.ClusterRecouple,
              //  das ein ClusterPassiveLost auslöst, mit ClusterCouplingPrepared infolge.
              //  Dann können wir kein ClusterPassiveLost ausgeben.
              //  JournaledStatePersistence Lock in die Anwendungsebene (hier) heben
              //  -- Nicht nötig durch die Abfrage auf initialState ?
              // FIXME (2) Deadlock when called immediately after start of Controller, before Journal has been started ?
              //  persistence.currentState may not response GetJournalState.
              persistence.currentState/*???*/.map(_.clusterState).flatMap {
                case clusterState: Coupled =>
                  common.ifClusterWatchAllowsActivation(clusterState, passiveLost,
                    Task {
                      // If a concurrent persist-operation has not been completed,
                      // the persistence lock blocks us to write the ClusterPassiveLost event,
                      // resulting in a deadlock.
                      // With this message we notify about the following ClusterPassiveLost event,
                      // JournalActor commits and complete the concurrent persist operation,
                      // and the lock will be released.
                      logger.debug(s"JournalActor.Input.PassiveLost($passiveLost)")
                      journalActor ! JournalActor.Input.PassiveLost(passiveLost)
                    } >>
                      persist(
                        {
                          case `initialState` => Right(Some(passiveLost))
                          case _ => Right(None)  // Ignore when ClusterState has changed
                        },
                        suppressClusterWatch = true/*already notified*/
                      ) .map(_.toCompleted)
                        .map(_.map((_: Completed) => true))
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

  private def fetchAndHandleAcknowledgedEventIds(passiveId: NodeId, passiveUri: Uri, after: EventId): Task[Checked[Completed]] =
    Task {
      logger.info(s"Fetching acknowledgements from passive cluster node, after=${EventId.toString(after)}")
    } >>
      Observable
        .fromResource(
          AkkaHttpControllerApi.resource(passiveUri, clusterConf.userAndPassword, httpsConfig, name = "acknowledgements")(actorSystem))
        .flatMap(api =>
          observeEventIds(api, after = after)
            //.map { eventId => logger.trace(s"$eventId acknowledged"); eventId }
            .whileBusyBuffer(OverflowStrategy.DropOld(bufferSize = 2))
            .detectPauses(clusterConf.heartbeat + clusterConf.failAfter)
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
                    .map(completed => Right(eventId))
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

  private def observeEventIds(api: HttpControllerApi, after: EventId): Observable[EventId] =
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
                heartbeat = Some(clusterConf.heartbeat))
            ) .onErrorRecover {
                case t: TornException => Left(t.problem)
              }
              .flatMap {
                case Left(torn: EventSeqTornProblem) =>
                  logger.debug(s"observeEventIds: $torn")
                  Task.pure(Left(torn.tornEventId)).delayExecution(1.s/*!!!*/)  // Repeat with last avaiable EventId
                case o => Task.pure(Right(o))
              })
        },
        stopRequested = () => stopRequested)

  def onTerminatedUnexpectedly: Task[Checked[Completed]] =
    Task.fromFuture(fetchingAcksTerminatedUnexpectedlyPromise.future)

  private def persist(toEvents: ClusterState => Checked[Option[ClusterEvent]], suppressClusterWatch: Boolean = false)
  : Task[Checked[(Seq[Stamped[KeyedEvent[ClusterEvent]]], ClusterState)]] =
    ( for {
        persisted <- EitherT(persistence.persistTransaction[ClusterEvent](NoKey)(state => toEvents(state.clusterState).map(_.toSeq)))
        _ <- EitherT.right(logPersisted(persisted._1.lastOption, persisted._2.clusterState))
        (stampedEvents, controllerState) = persisted
        clusterState = controllerState.clusterState
        _ <- EitherT(
          if (suppressClusterWatch || stampedEvents.isEmpty)
            Task.pure(Right(Completed))
          else
            clusterWatchSynchronizer.applyEvents(stampedEvents.map(_.value.event), clusterState))
      } yield (stampedEvents, clusterState)
    ).value

  private def logPersisted(maybeStampedEvent: Option[Stamped[KeyedEvent[ClusterEvent]]], clusterState: ClusterState) =
    Task {
      for (stamped <- maybeStampedEvent) {
        logger.info(clusterEventAndStateToString(stamped.value.event, clusterState))
      }
    }

  private object clusterWatchSynchronizer
  {
    private val heartbeatSender = SerialCancelable()
    // TODO Queue not more than one ClusterWatchMessage (with limited Observable?)
    private val lock = LockResource()

    def stop(): Unit =
      heartbeatSender.cancel()

    def stopHeartbeats(): Task[Unit] =
      lock.use(_ => Task {
        heartbeatSender := Cancelable.empty
      })

    def applyEvents(events: Seq[ClusterEvent], clusterState: ClusterState): Task[Checked[Completed]] =
      if (events.lengthIs == 1 && events.head.isInstanceOf[ClusterSwitchedOver])
        Task.pure(Checked(Completed))  // FailedOver event is reported by the newly active node
      else
        lock.use(_ =>
          Task {
            heartbeatSender := Cancelable.empty
          } >>
            clusterWatch.retryUntilReachable()(
              clusterWatch.applyEvents(from = ownId, events, clusterState)
            ) .map(_.map { completed =>
                scheduleHeartbeats(clusterState, delay = true)
                completed
              }))

    def scheduleHeartbeats(clusterState: ClusterState, delay: Boolean = false): Unit =
      heartbeatSender := (clusterState match {
        case clusterState: HasNodes if clusterState.activeId == ownId =>
          sendHeartbeats
            .delayExecution(if (delay) clusterConf.heartbeat else Duration.Zero)
            .runToFuture
            .andThen {
              case Success(Completed) =>
              case Failure(t) =>
                logger.warn(s"Error when sending heartbeat to ClusterWatch: ${t.toStringWithCauses}")
                logger.debug(s"Error when sending heartbeat to ClusterWatch: $t", t)
            }
        case _ =>
          Cancelable.empty
      })

    private def sendHeartbeats: Task[Completed] = {
      @volatile var cancelled = false

      Task.deferFutureAction { implicit scheduler =>
        val promise = Promise[Completed]()

        def doAHeartbeat: Task[Option[MonixDeadline]] =
          lock.use(_ =>
            (persistence.waitUntilStarted >> _currentClusterState).flatMap {
              case clusterState: ClusterState.HasNodes if clusterState.activeId == ownId && !cancelled =>
                Task(now).flatMap(since =>
                  clusterWatch.retryUntilReachable()(
                    clusterWatch.heartbeat(from = ownId, clusterState)
                  ) .materializeIntoChecked
                    .flatMap { checked =>
                      Task.now {
                        for (problem <- checked.left) {
                          handleProblem(problem)
                        }
                        Some(since)  // Continue
                      }
                    })
              case _ => Task.pure(None)  // Terminate
            }
          )

        def handleProblem(problem: Problem): Unit = {
          if (problem is ClusterWatchHeartbeatFromInactiveNodeProblem) {
            haltJava(s"EMERGENCY STOP due to: $problem", restart = true)  // TODO How to test
          }
          // Ignore other errors and continue
          val msg = s"ClusterWatch heartbeat: $problem"
          if (cancelled) logger.debug(msg) else logger.warn(msg)
        }

        def go(): Unit =
          doAHeartbeat
            .map {
              case Some(since) =>
                scheduler.scheduleOnce(clusterConf.heartbeat - since.elapsed) {
                  go()
                }

              case None =>
                promise.success(Completed)
            }
            .runToFuture
              .onComplete {
                case Failure(t) =>
                  if (!cancelled) {
                    logger.warn(s"sendHeartbeats: ${t.toStringWithCauses}",
                      if (t.isInstanceOf[AskTimeoutException]) null else t.nullIfNoStackTrace)
                  }
                case Success(_) =>
              }

        go()
        promise.future
      }.doOnCancel(Task {
        logger.debug("sendHeartbeats cancelled")
        cancelled = true
      })
    }
  }

  ///** Returns the current or at start the recovered ClusterState.
  //  * Required for the /api/cluster web service used by the restarting active node
  //  * asking the peer about its current (maybe failed-over) ClusterState. */
  //private def currentClusterState: Task[ClusterState] =
  //  Task.deferFuture {
  //    if (!started.future.isCompleted) logger.debug("currentClusterState: waiting for start")
  //    started.future
  //  } >>
  //    _currentClusterState

  def isActive: Task[Boolean] =
    Task.pure(activated)
    // Blocks if passive ???
    //currentClusterState.map {
    //  case Empty =>
    //    !clusterConf.isBackup
    //  case o: HasNodes =>
    //    o.activeId == clusterConf.ownId
    //}

  // Used to RegisterMe actor in JournalActor
  def journalingActor = persistence.actor
}

object Cluster
{
  private val logger = Logger(getClass)
}

package com.sos.jobscheduler.master.cluster

import akka.actor.ActorSystem
import akka.pattern.{AskTimeoutException, ask}
import akka.util.Timeout
import cats.data.EitherT
import cats.effect.ExitCase
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.eventbus.EventPublisher
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.monixutils.MonixBase.syntax._
import com.sos.jobscheduler.base.monixutils.MonixDeadline
import com.sos.jobscheduler.base.monixutils.MonixDeadline.now
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.{LockResource, SetOnce}
import com.sos.jobscheduler.base.web.{HttpClient, Uri}
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.http.RecouplingStreamReader
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.cluster.ClusterWatch.ClusterWatchHeartbeatFromInactiveNodeProblem
import com.sos.jobscheduler.core.cluster.HttpClusterWatch
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.recover.{JournaledStateRecoverer, Recovered}
import com.sos.jobscheduler.core.event.journal.{JournalActor, JournalConf}
import com.sos.jobscheduler.core.event.state.JournaledStatePersistence
import com.sos.jobscheduler.core.problems.MissingPassiveClusterNodeHeartbeatProblem
import com.sos.jobscheduler.core.startup.Halt.haltJava
import com.sos.jobscheduler.data.cluster.ClusterCommand.{ClusterInhibitActivation, ClusterStartBackupNode}
import com.sos.jobscheduler.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterCouplingPrepared, ClusterNodesAppointed, ClusterPassiveLost, ClusterSwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterState.{Coupled, CoupledActiveShutDown, Decoupled, Empty, FailedOver, HasNodes, NodesAppointed, PassiveLost, PreparedToBeCoupled}
import com.sos.jobscheduler.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeId, ClusterState}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeqTornProblem, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import com.sos.jobscheduler.master.cluster.Cluster._
import com.sos.jobscheduler.master.cluster.ClusterCommon.truncateFile
import com.sos.jobscheduler.master.cluster.ObservablePauseDetector._
import com.sos.jobscheduler.master.data.MasterCommand.InternalClusterCommand
import com.sos.jobscheduler.master.data.MasterState
import com.typesafe.config.{Config, ConfigUtil}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Paths}
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import monix.reactive.{Observable, OverflowStrategy}
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.util.{Failure, Success}

final class Cluster(
  journalMeta: JournalMeta,
  persistence: JournaledStatePersistence[MasterState],
  masterId: MasterId,
  journalConf: JournalConf,
  val clusterConf: ClusterConf,
  config: Config,
  eventIdGenerator: EventIdGenerator,
  testEventPublisher: EventPublisher[Any])
  (implicit journalActorAskTimeout: Timeout,
    scheduler: Scheduler,
    actorSystem: ActorSystem)
{
  import clusterConf.ownId

  private val journalActor = persistence.journalActor
  private val activationInhibitor = new ActivationInhibitor
  private val fetchingAcks = AtomicBoolean(false)
  private val fetchingAcksCancelable = SerialCancelable()
  private val fetchingAcksTerminatedUnexpectedlyPromise = Promise[Checked[Completed]]()
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
      userAndPassword = config.optionAs[SecretString]("jobscheduler.auth.agents." + ConfigUtil.joinPath(uri.string))
        .map(password => UserAndPassword(masterId.toUserId, password)),
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
    * - `(Task[None], _)` if this node starts as an active node
    * - `(Task[Some[MasterState]], _)` with the other's node MasterState if this node starts as a passive node.
    * - `(_, Task[Checked[ClusterFollowUp]]` when this node should be activated or terminated
    * @return A pair of `Task`s with maybe the current `MasterState` of this passive node (if so)
    *         and ClusterFollowUp.
    */
  def start(recovered: Recovered[MasterState], recoveredState: MasterState)
  : (Task[Option[MasterState]], Task[Checked[ClusterFollowUp[MasterState]]]) = {
    if (recovered.clusterState != Empty) {
      logger.info(s"Recovered ClusterState is ${recovered.clusterState}")
    }
    val (passiveState, followUp) = startCluster(recovered, recoveredState)
    _currentClusterState = passiveState.map(_.fold(recovered.clusterState)(_.clusterState))
    started.success(())
    passiveState ->
      followUp.map(_.map { case (clusterState, followUp) =>
        activated = followUp.isInstanceOf[ClusterFollowUp.BecomeActive[_]]
        clusterWatchSynchronizer.scheduleHeartbeats(clusterState)
        followUp
      })
  }

  private def startCluster(
    recovered: Recovered[MasterState],
    recoveredState: MasterState)
  : (Task[Option[MasterState]], Task[Checked[(ClusterState, ClusterFollowUp[MasterState])]])
  = {
    (recovered.clusterState, recovered.recoveredJournalFile) match {
      case (Empty, _) =>
        if (!clusterConf.isBackup) {
          logger.debug(s"Active primary cluster node '$ownId', still with no passive node appointed")
          Task.pure(None) ->
            (activationInhibitor.startActive >>
              Task.pure(Right(recovered.clusterState -> ClusterFollowUp.BecomeActive(recovered))))
        } else {
          logger.info(s"Backup cluster node '$ownId', awaiting appointment from a primary node")
          val promise = Promise[ClusterStartBackupNode]()
          expectingStartBackupCommand := promise
          val passiveClusterNode = Task.deferFuture(promise.future)
            .map(cmd => newPassiveClusterNode(cmd.idToUri, cmd.activeId, recovered))
            .memoize
          passiveClusterNode.flatMap(_.state.map(Some.apply)) ->
            passiveClusterNode.flatMap(passive =>
              activationInhibitor.startPassive >>
                passive.run(recoveredState))
        }

      case (recoveredClusterState: HasNodes, Some(recoveredJournalFile))
        if recoveredClusterState.activeId == ownId =>
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
                proceed(recoveredClusterState, recovered.eventId)
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
                  logger.debug("Recovering again due to shortend journal after failover")
                  trunkRecovered = JournaledStateRecoverer.recover[MasterState](
                    journalMeta, MasterState.Undefined, recovered.newStateBuilder, config /*, runningSince=???*/)
                  val truncatedRecoveredJournalFile = trunkRecovered.recoveredJournalFile
                    .getOrElse(sys.error(s"Unrecoverable journal file '${file.getFileName}''"))
                  assertThat(truncatedRecoveredJournalFile.state.clusterState == recoveredClusterState)
                  assertThat(truncatedRecoveredJournalFile.journalPosition == failedAt,
                    s"${truncatedRecoveredJournalFile.journalPosition} != $failedAt")
                }
                Some(recoveredClusterState ->
                  newPassiveClusterNode(otherFailedOver.idToUri, otherFailedOver.activeId, trunkRecovered,
                   otherFailedOver = true))

              case otherState =>
                sys.error(s"The other node is in invalid failedOver=$otherState")  // ???
            }.memoize

            failedOver.flatMap {
              case None => Task.pure(None)
              case Some((_, passiveClusterNode)) => passiveClusterNode.state map Some.apply
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
        passive.state.map(Some.apply) ->
          passive.run(recoveredState)

      case _ =>
        Task.pure(None) ->
          Task.pure(Left(Problem.pure(s"Unexpected clusterState=${recovered.clusterState} (id=$ownId)")))
    }
  }

  private def newPassiveClusterNode(
    idToUri: Map[ClusterNodeId, Uri],
    activeId: ClusterNodeId,
    recovered: Recovered[MasterState],
    otherFailedOver: Boolean = false)
  : PassiveClusterNode[MasterState]
  = {
    val common = new ClusterCommon(activationInhibitor, clusterWatch, clusterConf, testEventPublisher)
    new PassiveClusterNode(ownId, idToUri, activeId, journalMeta, recovered,
      otherFailedOver, journalConf, clusterConf, eventIdGenerator, common)
  }

  private def automaticallyAppointConfiguredBackupNode: Task[Checked[Completed]] =
    clusterConf.maybeIdToUri match {
      case Some(isToUri) =>
        persist {
          case Empty =>
            ClusterNodesAppointed.checked(isToUri, ownId).map(_ :: Nil)
          case _ =>
            Right(Nil)
        }.map(_.map { case (stampedEvents, state) =>
          for (last <- stampedEvents.lastOption) {
            proceed(state, last.eventId)
          }
          Completed
        })
      case _ => Task.pure(Right(Completed))
    }

  def appointNodes(idToUri: Map[ClusterNodeId, Uri], activeId: ClusterNodeId): Task[Checked[Completed]] =
    persist {
      case Empty =>
        ClusterNodesAppointed.checked(idToUri, activeId).map(_ :: Nil)
      case NodesAppointed(`idToUri`, `activeId`) =>
        Right(Nil)
      case clusterState: HasNodes =>
        if (clusterState.idToUri == idToUri)
          Right(Nil)
        else
          Left(Problem(s"Different Cluster Nodes have already been appointed"))
    }.map(_.map { case (stampedEvents, state) =>
      for (last <- stampedEvents.lastOption) {
        proceed(state, last.eventId)
      }
      Completed
    })

  def executeCommand(command: ClusterCommand): Task[Checked[ClusterCommand.Response]] =
    command match {
      case command: ClusterCommand.ClusterStartBackupNode =>
        Task {
          expectingStartBackupCommand.toOption match {
            case None => Left(Problem("Cluster node is not ready to accept a backup node configuration"))
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
              Right(ClusterCouplingPrepared(activeId) :: Nil)

            case clusterState: PreparedToBeCoupled
              if clusterState.activeId == activeId  && clusterState.passiveId == passiveId =>
              logger.debug(s"PreparedToBeCoupled ignored in clusterState=$clusterState")
              Right(Nil)

            case clusterState: Coupled
              if clusterState.activeId == activeId && clusterState.passiveId == passiveId =>
              logger.debug(s"ClusterPrepareCoupling ignored in clusterState=$clusterState")
              Right(Nil)

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
              Right(Nil)

            case s: PreparedToBeCoupled if s.activeId == activeId && s.passiveId == passiveId =>
              // This is the normally expected ClusterState
              Right(ClusterCoupled(activeId) :: Nil)

            case s: Coupled if s.activeId == activeId && s.passiveId == passiveId =>
              // Already coupled
              Right(Nil)

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
                Right(ClusterPassiveLost(passiveId) :: Nil)

              case _ =>
                Right(Nil)
            }
        ).map(_.map(_ => ClusterCommand.Response.Accepted))

      case ClusterCommand.ClusterInhibitActivation(duration) =>
        import ClusterCommand.ClusterInhibitActivation.Response
        activationInhibitor.inhibitActivation(duration)
          .flatMap {
            case Left(problem) => Task.pure(Left(problem))
            case Right(/*inhibited=*/true) => Task.pure(Right(Response(None)))
            case Right(/*inhibited=*/false) =>
              // Could not inhibit, so this node is already active
              currentClusterState.map {
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
    if (remainingActiveAfterRestart)
      inhibitActivationOfPeer.map {
        case Some(otherFailedOver) =>
          Left(Problem.pure(s"While activating this node, the other node has failed-over: $otherFailedOver"))
        case None =>
          Right(Completed)
      }
    else
      Task.pure(Right(Completed))

  def afterJounalingStarted: Task[Checked[Completed]] =
    Task {
      _currentClusterState = persistence.currentState.map(_.clusterState)
    } >>
      ( for {
          _ <- EitherT(automaticallyAppointConfiguredBackupNode)
          x <- EitherT(onRestartActiveNode)
        } yield x
      ).value

  private def onRestartActiveNode: Task[Checked[Completed]] =
    persistence.currentState.map(_.clusterState).flatMap {
      case state: CoupledActiveShutDown if state.activeId == ownId =>
        persist(_ => Right(ClusterActiveNodeRestarted :: Nil))
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
      AkkaHttpMasterApi.resource(uri, clusterConf.userAndPassword, name = "ClusterInhibitActivation")
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
          Right(ClusterSwitchedOver(coupled.passiveId) :: Nil)
        case state =>
          Left(Problem.pure(s"Switchover is possible only for the active cluster node," +
            s" but cluster state is: $state"))
      } .map(_.map { case (_: Seq[Stamped[_]], _) =>
          switchoverAcknowledged = true
          Completed
        })
        .guaranteeCase {
          case ExitCase.Error(_) =>
            currentClusterState.flatMap(clusterState =>  // TODO Lock currentClusterState (instead in JournalStatePersistence)
              Task { clusterWatchSynchronizer.scheduleHeartbeats(clusterState) })
          case _ => Task.unit
        }

  def shutDownThisNode: Task[Checked[Completed]] =
    clusterWatchSynchronizer.stopHeartbeats() >>
      persist {
        case coupled: Coupled if coupled.activeId == ownId =>
          Right(ClusterActiveNodeShutDown :: Nil)
        case _ =>
          Right(Nil)
      } .map(_.map((_: (Seq[Stamped[_]], _)) => Completed))
        .guaranteeCase {
          case ExitCase.Error(_) =>
            currentClusterState.flatMap(clusterState =>  // TODO Lock currentClusterState (instead in JournalStatePersistence)
              Task { clusterWatchSynchronizer.scheduleHeartbeats(clusterState) })
          case _ => Task.unit
        }

  private def proceed(state: ClusterState, eventId: EventId): Unit =
    state match {
      case state: NodesAppointed if state.activeId == ownId =>
        val common = new ClusterCommon(activationInhibitor, clusterWatch, clusterConf, testEventPublisher)
        common.tryEndlesslyToSendCommand(
          state.passiveUri,
          ClusterStartBackupNode(state.idToUri, activeId = ownId)
        ) .onErrorRecover { case t: Throwable =>
            logger.warn(s"Sending Cluster command to other node failed: $t", t)
          }
          .runAsyncAndForget

      case state: Coupled =>
        if (state.activeId == ownId) {
          fetchAndHandleAcknowledgedEventIds(state, eventId)
        } else
          sys.error(s"proceed: Unexpected ClusterState $state")

      case _ =>
    }

  private def fetchAndHandleAcknowledgedEventIds(initialState: Coupled, eventId: EventId): Unit =
    if (!fetchingAcks.getAndSet(true)) {
      def msg = s"observeEventIds(${initialState.passiveUri}, after=$eventId, userAndPassword=${clusterConf.userAndPassword})"
      val future: CancelableFuture[Checked[Completed]] =
        fetchAndHandleAcknowledgedEventIds(initialState.passiveId, initialState.passiveUri, after = eventId)
          .flatMap {
            case Left(missingHeartbeatProblem @ MissingPassiveClusterNodeHeartbeatProblem(id)) =>
              logger.warn("No heartbeat from passive cluster node - continuing as single active cluster node")
              assertThat(id != ownId)
              val passiveLost = ClusterPassiveLost(id)
              val common = new ClusterCommon(activationInhibitor, clusterWatch, clusterConf, testEventPublisher)
              // FIXME Exklusiver Zugriff (Lock) wegen parallelen ClusterCommand.ClusterRecouple,
              //  das ein ClusterPassiveLost auslöst, mit ClusterCouplingPrepared infolge.
              //  Dann können wir kein ClusterPassiveLost ausgeben.
              //  JournaledStatePersistence Lock in die Anwendungsebene (hier) heben
              //  -- Nicht nötig durch die Abfrage auf initialState ?
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
                          case `initialState` => Right(passiveLost :: Nil)
                          case _ => Right(Nil)  // Ignore when ClusterState has changed
                        },
                        suppressClusterWatch = true/*already notified*/
                      ) .map(_.toCompleted)
                        .map(_.map { (_: Completed) =>
                          fetchingAcks := false  // Allow fetching acknowledgements again when recoupling
                          true
                        })
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

  private def fetchAndHandleAcknowledgedEventIds(passiveId: ClusterNodeId, passiveUri: Uri, after: EventId): Task[Checked[Completed]] =
    Task {
      logger.info(s"Fetching acknowledgements from passive cluster node, after=${EventId.toString(after)}")
    } >>
      Observable
        .fromResource(
          AkkaHttpMasterApi.resource(passiveUri, clusterConf.userAndPassword, name = "acknowledgements")(actorSystem))
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
                    //.map(Right.apply)
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

  private def observeEventIds(api: HttpMasterApi, after: EventId): Observable[EventId] =
    RecouplingStreamReader
      .observe[EventId, EventId, HttpMasterApi](
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
            ).flatMap {
              case Left(torn: EventSeqTornProblem) =>
                logger.debug(s"observeEventIds: $torn")
                Task.pure(Left(torn.tornEventId)).delayExecution(1.s/*!!!*/)  // Repeat with last avaiable EventId
              case o => Task.pure(Right(o))
            })
        },
        stopRequested = () => stopRequested)

  def onTerminatedUnexpectedly: Task[Checked[Completed]] =
    Task.fromFuture(fetchingAcksTerminatedUnexpectedlyPromise.future)

  private def persist(toEvents: ClusterState => Checked[Seq[ClusterEvent]], suppressClusterWatch: Boolean = false)
  : Task[Checked[(Seq[Stamped[KeyedEvent[ClusterEvent]]], ClusterState)]] =
    ( for {
        persisted <- EitherT(persistence.persistTransaction[ClusterEvent](NoKey)(state => toEvents(state.clusterState)))
        _ <- EitherT(logPersisted(persisted._1, persisted._2))
        (stampedEvents, masterState) = persisted
        clusterState = masterState.clusterState
        _ <- EitherT(
          if (suppressClusterWatch || stampedEvents.isEmpty)
            Task.pure(Right(Completed))
          else
            clusterWatchSynchronizer.applyEvents(stampedEvents.map(_.value.event), clusterState))
      } yield (stampedEvents, clusterState)
    ).value

  private def logPersisted(stampedEvents: Seq[Stamped[AnyKeyedEvent]], state: MasterState) =
    Task {
      for (e <- stampedEvents) logger.info(s"ClusterEvent: ${e.value.event}")
      logger.info(s"ClusterState is ${state.clusterState}")
      Checked(())
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
            clusterWatch.retryable(
              clusterWatch.applyEvents(from = ownId, events, clusterState)
            ) .map(_.map { completed =>
                scheduleHeartbeats(clusterState)
                completed
              }))

    def scheduleHeartbeats(clusterState: ClusterState): Unit = {
      heartbeatSender := (clusterState match {
        case clusterState: HasNodes if clusterState.activeId == ownId =>
          val future = sendHeartbeats.runToFuture
          future onComplete {
            case Success(Completed) =>
            case Failure(t) =>
              logger.warn(s"Error when sending heartbeat to ClusterWatch: ${t.toStringWithCauses}")
              logger.debug(s"Error when sending heartbeat to ClusterWatch: $t", t)
          }
          future
        case _ =>
          Cancelable.empty
      })
    }

    private def sendHeartbeats: Task[Completed] = {
      @volatile var cancelled = false

      Task.deferFutureAction { implicit scheduler =>
        val promise = Promise[Completed]()

        def doAHeartbeat: Task[Option[MonixDeadline]] =
          lock.use(_ =>
            currentClusterState.flatMap {
              case clusterState: ClusterState.HasNodes if clusterState.activeId == ownId && !cancelled =>
                Task(now).flatMap(since =>
                  clusterWatch.retryable(
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

  /** Returns the current or at start the recovered ClusterState.
    * Required for the /api/cluster web service used by the restarting active node
    * asking the peer about its current (maybe failed-over) ClusterState. */
  def currentClusterState: Task[ClusterState] =
    Task.deferFuture {
      if (!started.future.isCompleted) logger.debug("currentClusterState: waiting for start")
      started.future
    } >>
      _currentClusterState

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

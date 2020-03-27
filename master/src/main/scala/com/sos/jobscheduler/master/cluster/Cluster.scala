package com.sos.jobscheduler.master.cluster

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import cats.data.EitherT
import cats.effect.ExitCase
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.eventbus.EventBus
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.LockResource
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.http.AkkaHttpUtils._
import com.sos.jobscheduler.common.http.{AkkaHttpClient, RecouplingStreamReader}
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.core.cluster.ClusterWatch.ClusterWatchHeartbeatFromInactiveNodeProblem
import com.sos.jobscheduler.core.cluster.HttpClusterWatch
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.recover.{JournaledStateRecoverer, Recovered}
import com.sos.jobscheduler.core.event.state.JournaledStatePersistence
import com.sos.jobscheduler.core.problems.MissingPassiveClusterNodeHeartbeatProblem
import com.sos.jobscheduler.core.startup.Halt.haltJava
import com.sos.jobscheduler.data.cluster.ClusterEvent.{Coupled, CouplingPrepared, NodesAppointed, PassiveLost, SwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterNodeRole.{Backup, Primary}
import com.sos.jobscheduler.data.cluster.ClusterState.{ClusterCoupled, ClusterEmpty, ClusterFailedOver, ClusterNodesAppointed, ClusterPassiveLost, ClusterPreparedToBeCoupled, Decoupled, HasNodes}
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterNodeRole, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.MasterState
import com.sos.jobscheduler.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import com.sos.jobscheduler.master.cluster.Cluster._
import com.sos.jobscheduler.master.cluster.ClusterCommon.truncateFile
import com.sos.jobscheduler.master.cluster.ObservablePauseDetector._
import com.sos.jobscheduler.master.data.MasterCommand.{ClusterInhibitActivation, ClusterStartBackupNode}
import com.typesafe.config.{Config, ConfigUtil}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Paths}
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import monix.reactive.{Observable, OverflowStrategy}
import scala.collection.immutable.Seq
import scala.concurrent.Promise
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.{Failure, Success}

final class Cluster(
  journalMeta: JournalMeta,
  persistence: JournaledStatePersistence[ClusterState, ClusterEvent],
  masterId: MasterId,
  clusterConf: ClusterConf,
  config: Config,
  eventIdGenerator: EventIdGenerator,
  testEventBus: EventBus)
  (implicit journalActorAskTimeout: Timeout,
    scheduler: Scheduler,
    actorSystem: ActorSystem)
{
  import clusterConf.{role => ownRole}

  private val journalActor = persistence.journalActor
  private val activationInhibitor = new ActivationInhibitor
  private val fetchingAcks = AtomicBoolean(false)
  @volatile
  private var fetchingAcksCancelable = SerialCancelable()
  private val fetchingAcksTerminatedUnexpectedlyPromise = Promise[Checked[Completed]]()
  @volatile
  private var switchoverAcknowledged = false
  @volatile
  private var stopRequested = false
  private val started = Promise[Unit]()
  @volatile
  private var _currentClusterState: Task[ClusterState] = null
  private var remainingActiveAfterRestart = false
  private val expectingStartBackupCommand = SetOnce[Promise[(Uri, Uri)]]
  private val ownUriOnce = new SetOnce[Uri]("ownUri")

  private lazy val clusterWatch = {
    val uri = clusterConf.agentUris.headOption
      .getOrElse(sys.error("Missing ClusterWatch / Agent URI in cluster configuration"))
    new HttpClusterWatch(
      uri.asAkka,
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
  def start(
    recovered: Recovered[MasterState, Event],
    recoveredClusterState: ClusterState,
    recoveredState: MasterState)
  : (Task[Option[MasterState]], Task[Checked[ClusterFollowUp[MasterState, Event]]]) = {
    if (recoveredClusterState != ClusterEmpty) {
      logger.debug(s"recoveredClusterState=$recoveredClusterState")
    }
    val (passiveState, followUp) = startCluster(recovered, recoveredClusterState, recoveredState, eventIdGenerator)
    _currentClusterState = passiveState flatMap {
      case None => persistence.currentState
      case Some(o) => Task.pure(o.clusterState)
    }
    started.success(())
    passiveState ->
      followUp.map(_.map { case (clusterState, followUp) =>
        _currentClusterState = persistence.currentState
        clusterWatchSynchronizer.scheduleHeartbeats(clusterState)
        persistence.start(clusterState)
        followUp
      })
  }

  private def startCluster(
    recovered: Recovered[MasterState, Event],
    recoveredClusterState: ClusterState,
    recoveredState: MasterState,
    eventIdGenerator: EventIdGenerator)
  : (Task[Option[MasterState]], Task[Checked[(ClusterState, ClusterFollowUp[MasterState, Event])]])
  = {
    setAndCheckOwnUri(recoveredClusterState).orThrow
    (recoveredClusterState, ownRole, recovered.recoveredJournalFile) match {
      case (ClusterEmpty, ClusterNodeRole.Primary, _) =>
        logger.info(s"Remaining the active primary cluster node, still without backup")
        Task.pure(None) ->
          (activationInhibitor.startActive >>
            Task.pure(Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered))))

      case (ClusterEmpty, Backup, _) =>
        logger.info(s"Remaining the still not appointed backup cluster node")
        val promise = Promise[(Uri, Uri)]()
        expectingStartBackupCommand := promise
        val passiveTask = Task.deferFuture(promise.future)
          .map { case (primaryUri, backupUri) =>
            newPassiveClusterNode(backupUri, primaryUri, recovered)
          }.memoize
        passiveTask.flatMap(_.state.map(Some.apply)) ->
          passiveTask.flatMap(passive => activationInhibitor.startPassive >>
            passive.run(recoveredClusterState, recoveredState))

      case (_, _, Some(recoveredJournalFile)) if recoveredClusterState.isActive(ownRole) =>
        recoveredClusterState match {
          case recoveredClusterState: ClusterCoupled =>
            import recoveredClusterState.passiveUri
            logger.info(s"This cluster node was coupled before restart - asking the other node '$passiveUri' about its state")
            val failedOver = inhibitActivationOf(passiveUri).map {
              case None =>
                // The other node has not failed-over
                logger.info("The other cluster is still passive, so this node remains the active cluster node")
                assertThat(recoveredClusterState.isActive(ownRole))
                remainingActiveAfterRestart = true
                proceed(recoveredClusterState, recovered.eventId)
                None

              case Some(otherFailedOver) =>
                import otherFailedOver.failedAt
                logger.warn(s"The other cluster node '${otherFailedOver.activeUri}' has become active (failed-over) while this node was absent")
                assertThat(otherFailedOver.activeUri == passiveUri &&
                           otherFailedOver.passiveUri == recoveredClusterState.activeUri)
                // This restarted, previously failed cluster node may have written one chunk of events more than
                // the passive node, maybe even an extra snapshot in a new journal file.
                // These extra events are not acknowledged. So we truncate the journal.
                var truncated = false
                val journalFiles = JournalFiles.listJournalFiles(journalMeta.fileBase) takeRight 2
                val journalFile =
                  if (journalFiles.last.afterEventId == failedAt.fileEventId)
                    journalFiles.last
                  else if (journalFiles.length == 2 &&
                    journalFiles.head.afterEventId == failedAt.fileEventId &&
                    journalFiles.last.afterEventId > failedAt.fileEventId)
                  {
                    logger.info(s"Removing journal file written after failover: ${recoveredJournalFile.file.getFileName}")
                    truncated = true
                    val deleteFile = journalFiles.last.file
                    Files.move(deleteFile, Paths.get(deleteFile + "~DELETED-AFTER-FAILOVER"), REPLACE_EXISTING)  // Keep the file for debugging
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
                  trunkRecovered = JournaledStateRecoverer.recover[MasterState, Event](
                    journalMeta, recovered.newStateBuilder, config /*, runningSince=???*/)
                  val truncatedRecoveredJournalFile = trunkRecovered.recoveredJournalFile
                    .getOrElse(sys.error(s"Unrecoverable journal file '${file.getFileName}''"))
                  assertThat(truncatedRecoveredJournalFile.state.clusterState == recoveredClusterState)
                  assertThat(truncatedRecoveredJournalFile.journalPosition == failedAt,
                    s"${truncatedRecoveredJournalFile.journalPosition} != $failedAt")
                }
                Some(recoveredClusterState ->
                  newPassiveClusterNode(otherFailedOver.passiveUri, otherFailedOver.activeUri, trunkRecovered,
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
                case Some((otherClusterState, passiveClusterNode)) =>
                  passiveClusterNode.run(otherClusterState, recoveredState)
              }

          case _ =>
            logger.info("Remaining the active cluster node without following node")
            proceed(recoveredClusterState, recovered.eventId)
            Task.pure(None) ->
              (activationInhibitor.startActive >>
                Task.pure(Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered))))
        }

      case (state: HasNodes, _, Some(_)) if state.isPassive(ownRole) =>
        logger.info(
          if (state.isCoupledPassiveRole(ownRole))
            s"Remaining a passive cluster node following the active node '${state.activeUri}'"
          else
            s"Remaining a passive cluster node trying to follow the active node '${state.activeUri}'")
        val passive = newPassiveClusterNode(state.passiveUri, state.activeUri, recovered)
        passive.state.map(Some.apply) ->
          passive.run(recoveredClusterState, recoveredState)

      case (_, _, _) =>
        Task.pure(None) ->
          Task.pure(Left(Problem.pure(s"This cluster node's own URI is not defined, but recovered clusterState=$recoveredClusterState")))

      case (_, _, _) =>
        Task.pure(None) ->
          Task.pure(Left(Problem.pure(s"Unexpected clusterState=$recoveredClusterState (role=$ownRole)")))
    }
  }

  private def newPassiveClusterNode(
    ownPassiveUri: Uri,
    activeUri: Uri,
    recovered: Recovered[MasterState, Event],
    otherFailedOver: Boolean = false)
  : PassiveClusterNode[MasterState]
  = {
    val common = new ClusterCommon(ownPassiveUri, activationInhibitor, clusterWatch, clusterConf, testEventBus)
    new PassiveClusterNode(ownPassiveUri, activeUri, journalMeta, recovered,
      otherFailedOver, clusterConf, eventIdGenerator, common)(actorSystem)
  }

  def automaticallyAppointConfiguredBackupNode(): Task[Checked[Completed]] =
    (ownRole, clusterConf.maybeUris) match {
      case (ClusterNodeRole.Primary, Some(uris)) =>
        persist {
          case ClusterEmpty =>
            NodesAppointed.checked(uris).map(_ :: Nil)
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

  def appointNodes(uris: Seq[Uri]): Task[Checked[Completed]] =
    persist {
      case ClusterEmpty =>
        NodesAppointed.checked(uris).map(_ :: Nil)
      case ClusterNodesAppointed(`uris`) =>
        Right(Nil)
      case state =>
        Left(Problem(s"A backup node can not be appointed while cluster is in state $state"))
    }.map(_.map { case (stampedEvents, state) =>
      for (last <- stampedEvents.lastOption) {
        proceed(state, last.eventId)
      }
      Completed
    })

  def prepareCoupling(activeUri: Uri, passiveUri: Uri): Task[Checked[Completed]] = {
    lazy val call = s"prepareCoupling(activeUri=$activeUri, passiveUri=$passiveUri)"
    persistence.waitUntilStarted.flatMap(_ =>
      persist {
        case ClusterNodesAppointed(Seq(`activeUri`, `passiveUri`)) =>
          Right(CouplingPrepared(passiveUri) :: Nil)

        case clusterState: ClusterPreparedToBeCoupled
          if clusterState.activeUri == activeUri && clusterState.passiveUri == passiveUri =>
          logger.debug(s"ClusterPreparedToBeCoupled ignored in clusterState=$clusterState")
          Right(Nil)

        case clusterState: ClusterCoupled
          if clusterState.activeUri == activeUri && clusterState.passiveUri == passiveUri =>
          logger.debug(s"ClusterPrepareCoupling ignored in clusterState=$clusterState")
          Right(Nil)

        case state: Decoupled if state.activeUri == activeUri && state.passiveUri == passiveUri =>
          Right(CouplingPrepared(passiveUri) :: Nil)

        case state =>
          Left(Problem.pure(s"$call rejected due to inappropriate cluster state $state"))
      }.map(_.map { case (stampedEvents, state) =>
        for (stamped <- stampedEvents.lastOption) proceed(state, stamped.eventId)
        Completed
      }))
    }

  def couple(activeUri: Uri, passiveUri: Uri): Task[Checked[Completed]] =
    persistence.waitUntilStarted >>
      EitherT(Task {
        checkIsOwnUri(activeUri)
      }).flatMap(_ => EitherT(
        persist {
          case s: ClusterPassiveLost
            if s.activeUri == activeUri && s.passiveUri == passiveUri =>
            // Happens when this active node has restarted just before the passive one
            // and has already issued an PassiveLost event
            // We ignore this.
            // The passive node will replicate PassiveLost event and recouple
            Right(Nil)

          case s: ClusterPreparedToBeCoupled
            if s.activeUri == activeUri && s.passiveUri == passiveUri =>
            // This is the normally expected ClusterState
            Right(Coupled :: Nil)

          case s: ClusterCoupled
            if s.activeUri == activeUri && s.passiveUri == passiveUri =>
            // Already coupled
            Right(Nil)

          case s =>
            Left(Problem.pure(s"couple(primaryUri=$activeUri, passiveUri=$passiveUri) rejected " +
              s"due to inappropriate cluster state $s"))
        }.map(_
          .map { case (stampedEvents, state) =>
            for (stamped <- stampedEvents.lastOption) proceed(state, stamped.eventId)
            Completed
          }))
      ).value

  def recouple(activeUri: Uri, passiveUri: Uri): Task[Checked[Completed]] =
    persistence.waitUntilStarted >>
      EitherT(Task {
        checkIsOwnUri(activeUri)
      }).flatMap(_ => EitherT(
          persist {
            case s: ClusterCoupled
              if s.activeUri == activeUri && s.passiveUri == passiveUri =>
              Right(PassiveLost(passiveUri) :: Nil)

            case _ =>
              Right(Nil)
          }.map(_.toCompleted))
      ).value

  def startBackup(primaryUri: Uri, backupUri: Uri): Task[Checked[Completed]] =
    Task {
      expectingStartBackupCommand.toOption match {
        case None => Left(Problem("Cluster node is not ready to accept a backup node configuration"))
        case Some(promise) =>
          promise.trySuccess(primaryUri -> backupUri)
          Right(Completed)
      }
    }

  def inhibitActivation(duration: FiniteDuration): Task[Checked[Option[ClusterFailedOver]]] =
    activationInhibitor.inhibitActivation(duration)
      .flatMap {
        case Left(problem) => Task.pure(Left(problem))
        case Right(/*inhibited=*/true) => Task.pure(Right(None))
        case Right(/*inhibited=*/false) =>
          // Could not inhibit, so this node is already active
          currentClusterState.map {
            case failedOver: ClusterFailedOver =>
              logger.debug(s"inhibitActivation(${duration.pretty}) => $failedOver")
              Right(Some(failedOver))
            case clusterState =>
              Left(Problem.pure(
                s"ClusterInhibitActivation command failed because node is already active but not failed-over: $clusterState"))
          }
      }

  def inhibitActivationOfPeer: Task[Option[ClusterFailedOver]] =
    _currentClusterState.flatMap {
      case clusterState: ClusterState.HasNodes if clusterState.isActive(ownRole) =>
        inhibitActivationOf(clusterState.passiveUri)

      case clusterState => Task.raiseError(new IllegalStateException(
        s"inhibitActivationOfPeer expects active cluster node, but clusterState=$clusterState"))
    }

  private def inhibitActivationOf(uri: Uri): Task[Option[ClusterFailedOver]] =
    Task.defer {
      val retryDelay = 5.s  // TODO
      val retryDelays = Iterator.single(1.s/*TODO*/) ++ Iterator.continually(retryDelay)
      AkkaHttpMasterApi.resource(uri, name = "ClusterInhibitActivation")
        .use(otherNode =>
          otherNode.loginUntilReachable(clusterConf.userAndPassword, retryDelays) >>
            otherNode.executeCommand(ClusterInhibitActivation(2 * clusterConf.failAfter/*TODO*/))
              .map(_.failedOver))
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
        case coupled: ClusterCoupled if coupled.isActive(ownRole) =>
          Right(SwitchedOver(coupled.passiveUri) :: Nil)
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

  private def proceed(state: ClusterState, eventId: EventId): Unit =
    state match {
      case state: ClusterNodesAppointed if ownRole == Primary =>
        val common = new ClusterCommon(state.primaryUri, activationInhibitor, clusterWatch, clusterConf, testEventBus)
        common.tryEndlesslyToSendCommand(state.roleToUri(Backup), ClusterStartBackupNode(state.uris))
          .onErrorRecover { case t: Throwable =>
            logger.warn(s"Sending Cluster command to other node failed: $t", t)
          }.runAsyncAndForget

      case state: ClusterCoupled =>
        if (state.isActive(ownRole)) {
          fetchAndHandleAcknowledgedEventIds(state, eventId)
        } else
          sys.error(s"proceed: Unexpected ClusterState $state")
      case _ =>
    }

  private def fetchAndHandleAcknowledgedEventIds(initialState: ClusterCoupled, eventId: EventId): Unit =
    if (!fetchingAcks.getAndSet(true)) {
      def msg = s"observeEventIds(${initialState.passiveUri}, after=$eventId, userAndPassword=${clusterConf.userAndPassword})"
      val future: CancelableFuture[Checked[Completed]] =
        fetchAndHandleAcknowledgedEventIds(initialState.passiveUri, after = eventId)
          .flatMap {
            case Left(missingHeartbeatProblem @ MissingPassiveClusterNodeHeartbeatProblem(uri)) =>
              logger.warn("No heartbeat from passive cluster node - continuing as single active cluster node")
              val passiveLost = PassiveLost(uri)
              val common = new ClusterCommon(initialState.activeUri, activationInhibitor, clusterWatch, clusterConf, testEventBus)
              // FIXME Exklusiver Zugriff (Lock) wegen parallelen MasterCommand.ClusterRecouple, das ein EventLost auslöst,
              //  mit CouplingPrepared infolge. Dann können wir kein PassiveLost ausgeben.
              //  JournaledStatePersistence Lock in die Anwendungsebene (hier) heben
              persistence.currentState.flatMap {
                case clusterState: ClusterCoupled =>
                  common.ifClusterWatchAllowsActivation(clusterState, passiveLost,
                    persist(_ => Right(passiveLost :: Nil), suppressClusterWatch = true/*already notified*/)
                      .map(_.toCompleted)
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
                case _ => Task.pure(Right(Completed))
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
          // Completes only when not canceled and then it is a failure
          fetchingAcksTerminatedUnexpectedlyPromise.complete(tried)
      }
    }

  private def fetchAndHandleAcknowledgedEventIds(uri: Uri, after: EventId): Task[Checked[Completed]] =
    Task {
      logger.info(s"Fetching acknowledgements from passive cluster node, after=${EventId.toString(after)}")
    } >>
      Observable
        .fromResource(
          AkkaHttpMasterApi.resource(uri, name = "acknowledgements")(actorSystem))
        .flatMap(api =>
          observeEventIds(api, after = after)
            .whileBusyBuffer(OverflowStrategy.DropOld(bufferSize = 2))
            .detectPauses(clusterConf.heartbeat + clusterConf.failAfter)
            .takeWhile(_ => !switchoverAcknowledged)  // Race condition: may be set too late
            .mapEval {
              case None => Task.pure(Left(MissingPassiveClusterNodeHeartbeatProblem(uri)))
              case Some(eventId) =>
                Task.deferFuture {
                  // Possible dead letter when `switchoverAcknowledged` is detected too late,
                  // because after JournalActor has committed SwitchedOver (after ack), JournalActor stops.
                  (journalActor ? JournalActor.Input.PassiveNodeAcknowledged(eventId = eventId)).mapTo[Completed]
                    .map(Right.apply)
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

  private def observeEventIds(api: HttpMasterApi, after: EventId): Observable[EventId] =
    RecouplingStreamReader
      .observe[EventId, EventId, HttpMasterApi](
        toIndex = identity,
        api,
        clusterConf.userAndPassword,
        clusterConf.recouplingStreamReader,
        after = after,
        getObservable = (after: EventId) => {
          val eventRequest = EventRequest.singleClass[Event](after = after, timeout = None)
          AkkaHttpClient.liftProblem(
            api.eventIdObservable(eventRequest, heartbeat = Some(clusterConf.heartbeat)))
        },
        stopRequested = () => stopRequested)

  def onTerminatedUnexpectedly: Task[Checked[Completed]] =
    Task.fromFuture(fetchingAcksTerminatedUnexpectedlyPromise.future)

  private def persist(toEvents: ClusterState => Checked[Seq[ClusterEvent]], suppressClusterWatch: Boolean = false)
  : Task[Checked[(Seq[Stamped[KeyedEvent[ClusterEvent]]], ClusterState)]] =
    (for {
      persisted <- EitherT(persistence.persistTransaction(NoKey)(toEvents))
      (stampedEvents, clusterState) = persisted
      _ <- EitherT(Task.pure(setAndCheckOwnUri(clusterState)))
      _ <- EitherT(
        if (suppressClusterWatch || stampedEvents.isEmpty)
          Task.pure(Right(Completed))
        else
          clusterWatchSynchronizer.applyEvents(stampedEvents.map(_.value.event), clusterState))
    } yield persisted).value

  private def setAndCheckOwnUri(clusterState: ClusterState): Checked[Unit] =
    clusterState.roleToMaybeUri(ownRole) match {
      case None => Checked.unit
      case Some(uri) =>
        ownUriOnce.trySet(uri)
        if (ownUriOnce.get == uri)
          Checked.unit
        else
          Left(Problem(
            s"Recovered clusterState=$clusterState does not match ownUri=$ownUriOnce"))
    }

  private def checkIsOwnUri(uri: Uri): Checked[Unit] =
    ownUriOnce.toOption match {
      case None | Some(`uri`) => Checked.unit
      case Some(u) => Left(Problem.pure(s"The node's URI is '$u', but it is used as '$uri'"))
    }

  private object clusterWatchSynchronizer
  {
    private val heartbeatSender = SerialCancelable()
    // TODO Queue not more than one ClusterWatchMessage (with limited Observable?)
    private val lock = LockResource()
    @volatile private var stopped = false

    def stop(): Unit = {
      stopped = true
      heartbeatSender.cancel()
    }

    def stopHeartbeats(): Task[Unit] =
      lock.use(_ => Task {
        stopped = true
        heartbeatSender := Cancelable.empty
      })

    def applyEvents(events: Seq[ClusterEvent], clusterState: ClusterState): Task[Checked[Completed]] =
      if (events.lengthCompare(1) == 0 && events.head.isInstanceOf[SwitchedOver])
        Task.pure(Checked(Completed))  // FailedOver event is reported by the newly active node
      else
        lock.use(_ =>
          Task {
            stopped = true
            heartbeatSender := Cancelable.empty
          } >>
            Task.defer(
              clusterState.roleToMaybeUri(ownRole) match {
                case None => Task.pure(Checked.completed)
                case Some(ownUri) =>
                  clusterWatch.applyEvents(from = ownUri, events, clusterState)
              })
        .map(_.map { completed =>
          scheduleHeartbeats(clusterState)
          completed
        }))

    def scheduleHeartbeats(clusterState: ClusterState): Unit = {
      stopped = false
      heartbeatSender := (clusterState match {
        case clusterState: HasNodes if clusterState.isActive(ownRole) =>
          scheduler.scheduleAtFixedRate(Duration.Zero, clusterConf.heartbeat) {
            lock.use(_ =>
              currentClusterState.flatMap {
                case clusterState: ClusterState.HasNodes if clusterState.isActive(ownRole) && !stopped =>  // check again
                  clusterWatch.heartbeat(from = clusterState.roleToUri(ownRole), clusterState)

                case _ =>
                  Task.pure(Right(Completed))
              }
            ).runToFuture onComplete {
              case Success(Right(Completed)) =>
              case Success(Left(problem)) if problem.codeOption contains ClusterWatchHeartbeatFromInactiveNodeProblem.code =>
                haltJava(s"EMERGENCY STOP due to: $problem", restart = true)  // TODO How to test?
              case Success(problem) =>
                logger.warn(s"Error when sending heartbeat to ClusterWatch: $problem")
              case Failure(t) =>
                logger.warn(s"Error when sending heartbeat to ClusterWatch: ${t.toStringWithCauses}")
                logger.debug(s"Error when sending heartbeat to ClusterWatch: $t", t)
            }
          }
        case _ =>
          Cancelable.empty
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

  def isRemainingActiveAfterRestart =
    remainingActiveAfterRestart

  // Used to RegisterMe actor in JournalActor
  def journalingActor = persistence.actor
}

object Cluster
{
  private val logger = Logger(getClass)
}

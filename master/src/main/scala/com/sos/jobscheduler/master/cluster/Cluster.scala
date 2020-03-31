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
import com.sos.jobscheduler.base.monixutils.MonixDeadline.now
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.{LockResource, SetOnce}
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.http.{AkkaHttpClient, RecouplingStreamReader}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops.RichCheckedTask
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
import com.sos.jobscheduler.data.cluster.ClusterEvent.{ClusterCoupled, ClusterCouplingPrepared, ClusterNodesAppointed, ClusterPassiveLost, ClusterSwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterState.{Coupled, Decoupled, Empty, FailedOver, HasNodes, NodesAppointed, PassiveLost, PreparedToBeCoupled}
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterNodeId, ClusterState}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.MasterState
import com.sos.jobscheduler.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import com.sos.jobscheduler.master.cluster.Cluster._
import com.sos.jobscheduler.master.cluster.ClusterCommon.truncateFile
import com.sos.jobscheduler.master.cluster.ObservablePauseDetector._
import com.sos.jobscheduler.master.data.MasterCommand
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
import scala.concurrent.duration._
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
  import clusterConf.ownId

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
  def start(
    recovered: Recovered[MasterState, Event],
    recoveredClusterState: ClusterState,
    recoveredState: MasterState)
  : (Task[Option[MasterState]], Task[Checked[ClusterFollowUp[MasterState, Event]]]) = {
    if (recoveredClusterState != Empty) {
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
    (recoveredClusterState, recovered.recoveredJournalFile) match {
      case (Empty, _) =>
        if (!clusterConf.isBackup) {
          logger.debug(s"Active primary cluster node '$ownId', still with no passive node appointed")
          Task.pure(None) ->
            (activationInhibitor.startActive >>
              Task.pure(Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered))))
        } else {
          logger.info(s"Backup cluster node '$ownId', awaiting appointment from a primary node")
          val promise = Promise[ClusterStartBackupNode]()
          expectingStartBackupCommand := promise
          val passiveTask = Task.deferFuture(promise.future)
            .map(cmd => newPassiveClusterNode(cmd.idToUri, cmd.activeId, recovered))
            .memoize
          passiveTask.flatMap(_.state.map(Some.apply)) ->
            passiveTask.flatMap(passive => activationInhibitor.startPassive >>
              passive.run(recoveredClusterState, recoveredState))
        }

      case (recoveredClusterState: HasNodes, Some(recoveredJournalFile))
        if recoveredClusterState.activeId == ownId =>
        recoveredClusterState match {
          case recoveredClusterState: Coupled =>
            import recoveredClusterState.{passiveId, passiveUri}
            logger.info(s"This cluster node '$ownId' was coupled before restart - asking '$passiveId' about its state")
            val failedOver = inhibitActivationOf(passiveUri).map {
              case None =>
                // The other node has not failed-over
                logger.info("The other cluster is still passive, so this node remains the active cluster node")
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

      case (state: HasNodes, Some(_)) if state.passiveId == ownId =>
        logger.info(
          if (state.isCoupledPassiveRole(ownId))
            s"Remaining a passive cluster node following the active node '${state.activeId}'"
          else
            s"Remaining a passive cluster node trying to follow the active node '${state.activeId}'")
        val passive = newPassiveClusterNode(state.idToUri, state.activeId, recovered)
        passive.state.map(Some.apply) ->
          passive.run(recoveredClusterState, recoveredState)

      case _ =>
        Task.pure(None) ->
          Task.pure(Left(Problem.pure(s"Unexpected clusterState=$recoveredClusterState (id=$ownId)")))
    }
  }

  private def newPassiveClusterNode(
    idToUri: Map[ClusterNodeId, Uri],
    activeId: ClusterNodeId,
    recovered: Recovered[MasterState, Event],
    otherFailedOver: Boolean = false)
  : PassiveClusterNode[MasterState]
  = {
    val common = new ClusterCommon(activationInhibitor, clusterWatch, clusterConf, testEventBus)
    new PassiveClusterNode(ownId, idToUri, activeId, journalMeta, recovered,
      otherFailedOver, clusterConf, eventIdGenerator, common)(actorSystem)
  }

  def automaticallyAppointConfiguredBackupNode(): Task[Checked[Completed]] =
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

  def executeCommand(command: MasterCommand): Task[Checked[MasterCommand.Response]] =
    command match {
      case MasterCommand.ClusterAppointNodes(idToUri, activeId) =>
        persist {
          case Empty =>
            ClusterNodesAppointed.checked(idToUri, activeId).map(_ :: Nil)
          case NodesAppointed(`idToUri`, `activeId`) =>
            Right(Nil)
          case state =>
            Left(Problem(s"A backup node can not be appointed while cluster is in state $state"))
        }.map(_.map { case (stampedEvents, state) =>
          for (last <- stampedEvents.lastOption) {
            proceed(state, last.eventId)
          }
          MasterCommand.Response.Accepted
        })

      case MasterCommand.ClusterPrepareCoupling(activeId, passiveId) =>
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
            MasterCommand.Response.Accepted
          }))

      case MasterCommand.ClusterCouple(activeId, passiveId) =>
        persistence.waitUntilStarted >>
          persist {
            case s: PassiveLost
              if s.activeId == activeId && s.passiveId == passiveId =>
              // Happens when this active node has restarted just before the passive one
              // and has already issued an PassiveLost event
              // We ignore this.
              // The passive node will replicate PassiveLost event and recouple
              Right(Nil)

            case s: PreparedToBeCoupled
              if s.activeId == activeId && s.passiveId == passiveId =>
              // This is the normally expected ClusterState
              Right(ClusterCoupled(activeId) :: Nil)

            case s: Coupled
              if s.activeId == activeId && s.passiveId == passiveId =>
              // Already coupled
              Right(Nil)

            case s =>
              Left(Problem.pure(s"$command command rejected due to inappropriate cluster state $s"))
          }.map(_
            .map { case (stampedEvents, state) =>
              for (stamped <- stampedEvents.lastOption) proceed(state, stamped.eventId)
              MasterCommand.Response.Accepted

            })

      case MasterCommand.ClusterRecouple(activeId, passiveId) =>
        persistence.waitUntilStarted >>
          persist {
            case s: Coupled
              if s.activeId == activeId && s.passiveId == passiveId =>
              Right(ClusterPassiveLost(passiveId) :: Nil)

            case _ =>
              Right(Nil)
          }.map(_.map(_ => MasterCommand.Response.Accepted))

      case command: MasterCommand.ClusterStartBackupNode =>
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
                Right(MasterCommand.Response.Accepted)
              }
          }
        }

      case MasterCommand.ClusterInhibitActivation(duration) =>
        import MasterCommand.ClusterInhibitActivation.Response
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

  def inhibitActivationOfPeer: Task[Option[FailedOver]] =
    _currentClusterState.flatMap {
      case clusterState: ClusterState.HasNodes if clusterState.activeId == ownId =>
        inhibitActivationOf(clusterState.passiveUri)

      case clusterState => Task.raiseError(new IllegalStateException(
        s"inhibitActivationOfPeer expects active cluster node, but clusterState=$clusterState"))
    }

  private def inhibitActivationOf(uri: Uri): Task[Option[FailedOver]] =
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

  private def proceed(state: ClusterState, eventId: EventId): Unit =
    state match {
      case state: NodesAppointed if state.activeId == ownId =>
        val common = new ClusterCommon(activationInhibitor, clusterWatch, clusterConf, testEventBus)
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
              assert(id != ownId)
              val passiveLost = ClusterPassiveLost(id)
              val common = new ClusterCommon(activationInhibitor, clusterWatch, clusterConf, testEventBus)
              // FIXME Exklusiver Zugriff (Lock) wegen parallelen MasterCommand.ClusterRecouple, das ein EventLost auslöst,
              //  mit ClusterCouplingPrepared infolge. Dann können wir kein PassiveLost ausgeben.
              //  JournaledStatePersistence Lock in die Anwendungsebene (hier) heben
              persistence.currentState/*???*/.flatMap {
                case clusterState: Coupled =>
                  common.ifClusterWatchAllowsActivation(clusterState, passiveLost,
                    persist(
                      {
                        case _: PassiveLost => Right(Nil)
                        case _ => Right(passiveLost :: Nil)
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
          // Completes only when not canceled and then it is a failure
          fetchingAcksTerminatedUnexpectedlyPromise.complete(tried)
      }
    }

  private def fetchAndHandleAcknowledgedEventIds(passiveId: ClusterNodeId, passiveUri: Uri, after: EventId): Task[Checked[Completed]] =
    Task {
      logger.info(s"Fetching acknowledgements from passive cluster node, after=${EventId.toString(after)}")
    } >>
      Observable
        .fromResource(
          AkkaHttpMasterApi.resource(passiveUri, name = "acknowledgements")(actorSystem))
        .flatMap(api =>
          observeEventIds(api, after = after)
            .whileBusyBuffer(OverflowStrategy.DropOld(bufferSize = 2))
            .detectPauses(clusterConf.heartbeat + clusterConf.failAfter)
            .takeWhile(_ => !switchoverAcknowledged)  // Race condition: may be set too late
            .mapEval {
              case None => Task.pure(Left(MissingPassiveClusterNodeHeartbeatProblem(passiveId)))
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
      _ <- EitherT(
        if (suppressClusterWatch || stampedEvents.isEmpty)
          Task.pure(Right(Completed))
        else
          clusterWatchSynchronizer.applyEvents(stampedEvents.map(_.value.event), clusterState))
    } yield persisted).value

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
      if (events.lengthCompare(1) == 0 && events.head.isInstanceOf[ClusterSwitchedOver])
        Task.pure(Checked(Completed))  // FailedOver event is reported by the newly active node
      else
        lock.use(_ =>
          Task {
            stopped = true
            heartbeatSender := Cancelable.empty
          } >>
            clusterWatch.applyEvents(from = ownId, events, clusterState)
              .map(_.map { completed =>
                scheduleHeartbeats(clusterState)
                completed
              }))

    def scheduleHeartbeats(clusterState: ClusterState): Unit = {
      stopped = false
      heartbeatSender := (clusterState match {
        case clusterState: HasNodes if clusterState.activeId == ownId =>
          val future = sendHeartbeats.runToFuture
          future onComplete {
            case Success(Right(Completed)) =>
            case Success(Left(problem)) if problem.codeOption contains ClusterWatchHeartbeatFromInactiveNodeProblem.code =>
              haltJava(s"EMERGENCY STOP due to: $problem", restart = true)  // TODO How to test?
            case Success(problem) =>
              logger.warn(s"Error when sending heartbeat to ClusterWatch: $problem")
            case Failure(t) =>
              logger.warn(s"Error when sending heartbeat to ClusterWatch: ${t.toStringWithCauses}")
              logger.debug(s"Error when sending heartbeat to ClusterWatch: $t", t)
          }
          future
        case _ =>
          Cancelable.empty
      })
    }

    private def sendHeartbeats: Task[Checked[Completed]] =
      Task.tailRecM(())(_ =>
        lock.use(_ =>
          currentClusterState.flatMap {
            case clusterState: ClusterState.HasNodes if clusterState.activeId == ownId && !stopped =>  // check again
              Task(now).flatMap(since =>
                clusterWatch.heartbeat(from = ownId, clusterState)
                  .materializeIntoChecked
                  .map { checked =>
                    for (problem <- checked.left) logger.warn(s"ClusterWatch heartbeat: $problem")
                    Left(since)  // Ignore error and continue
                  })

            case _ =>
              Task.pure(Right(Right(Completed)))
          })
        .flatMap {
          case Left(since) => Task.sleep(clusterConf.heartbeat - since.elapsed).map(_ => Left())
          case Right(o) => Task.pure(Right(o))
        })
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

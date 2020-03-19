package com.sos.jobscheduler.master.cluster

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import cats.data.EitherT
import cats.effect.ExitCase
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
import com.sos.jobscheduler.common.scalautil.Logger
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
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, BecameSole, Coupled, FollowerLost, FollowingStarted, SwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterState.{Decoupled, Empty, HasBackupNode, IsCoupled, NodesAreAppointed, Sole}
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
import com.sos.jobscheduler.master.data.MasterCommand
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
  private val journalActor = persistence.journalActor
  private val activationInhibitor = new ActivationInhibitor
  private val fetchingAcks = AtomicBoolean(false)
  @volatile
  private var fetchingAcksFuture: CancelableFuture[Checked[Completed]] = null
  private val fetchingEventsPromise = Promise[Checked[Completed]]()
  @volatile
  private var switchoverAcknowledged = false
  @volatile
  private var stopRequested = false
  private val started = Promise[Unit]()
  @volatile
  private var _currentClusterState: Task[ClusterState] = null
  private var remainingActiveAfterRestart = false

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
    for (o <- Option(fetchingAcksFuture)) {
      o.cancel()
    }
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
    if (recoveredClusterState != ClusterState.Empty) {
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
  =
    (recoveredClusterState, clusterConf.maybeOwnUri, clusterConf.maybeRole, recovered.recoveredJournalFile) match {
      case (Empty, _, None | Some(_: ClusterNodeRole.Primary), _) =>
        logger.info(s"Remaining the active primary cluster node, still without backup")
        Task.pure(None) ->
          Task.pure(Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered)))

      case (Empty, Some(ownUri), Some(ClusterNodeRole.Backup(activeUri)), _) =>
        logger.info(s"Remaining the (still not following) backup cluster node '$ownUri' for primary node at $activeUri")
        val passive = newPassiveClusterNode(ownUri, activeUri, recovered)
        passive.state.map(Some.apply) ->
          passive.run(recoveredClusterState, recoveredState)

      case (_, Some(ownUri), _, Some(recoveredJournalFile)) if recoveredClusterState.isActive(ownUri) =>
        recoveredClusterState match {
          case recoveredClusterState: IsCoupled =>
            import recoveredClusterState.passiveUri
            logger.info(s"This cluster node was coupled before restart - asking the other node '$passiveUri' about its state")
            val failedOver = inhibitActivationOf(passiveUri).map {
              case otherState: ClusterState if otherState.isActive(ownUri) =>
                logger.info("The other cluster is still passive: this node remains the active cluster node")
                assertThat(recoveredClusterState.isActive(ownUri))
                remainingActiveAfterRestart = true
                proceed(recoveredClusterState, recovered.eventId)
                None

              case otherState: ClusterState.IsFailedOver =>
                import otherState.failedAt
                logger.warn(s"The other cluster node '${otherState.activeUri}' has become active (failed-over) while this node was absent")
                assertThat(otherState.activeUri == passiveUri &&
                           otherState.passiveUri == recoveredClusterState.activeUri)
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
                      s" $otherState <-> ${journalFiles.map(_.file.getFileName).mkString(", ")}")
                assertThat(journalFile.afterEventId == failedAt.fileEventId)
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
                  // May take a long time !!!
                  // TODO Recovering may be omitted because the new active node has written a snapshot immediately after failover
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
                  newPassiveClusterNode(ownUri, otherState.activeUri, trunkRecovered, otherFailedOver = true))

              case otherState =>
                sys.error(s"The other node is in invalid clusterState=$otherState")  // ???
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
              Task.pure(Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered)))
        }

      case (state: HasBackupNode, Some(ownUri), _, Some(_)) if state.passiveUri == ownUri =>
        logger.info(
          if (state.isTheFollowingNode(ownUri))
            s"Remaining a passive cluster node following the active node '${state.activeUri}'"
          else
            s"Remaining a passive cluster node trying to follow the active node '${state.activeUri}'")
        val passive = newPassiveClusterNode(ownUri, state.activeUri, recovered)
        passive.state.map(Some.apply) ->
          passive.run(recoveredClusterState, recoveredState)

      case (_, None, _, _) =>
        Task.pure(None) ->
          Task.pure(Left(Problem.pure(s"This cluster node's own URI is not defined, but recovered clusterState=$recoveredClusterState")))

      case (_, Some(ownUri), _, _) =>
        Task.pure(None) ->
          Task.pure(Left(Problem.pure(s"Unexpected clusterState=$recoveredClusterState (ownUri=$ownUri)")))
    }

  private def newPassiveClusterNode(
    ownUri: Uri,
    activeUri: Uri,
    recovered: Recovered[MasterState, Event],
    otherFailedOver: Boolean = false)
  = {
    val common = new ClusterCommon(ownUri, activationInhibitor, clusterWatch, testEventBus)
    new PassiveClusterNode(ownUri, activeUri, journalMeta, recovered,
      otherFailedOver, clusterConf, eventIdGenerator, common)(actorSystem)
  }

  def automaticallyAppointConfiguredBackupNode(): Task[Checked[Completed]] =
    clusterConf.maybeRole match {
      case Some(ClusterNodeRole.Primary(Some(backupUri))) =>
        persist {
          case state @ (ClusterState.Empty | _: ClusterState.Sole /*| _: ClusterState.AwaitingAppointment*/) =>
            for (events <- becomeSoleIfEmpty(state)) yield
              events ::: BackupNodeAppointed(backupUri) :: Nil
          case _ =>
            Right(Nil)
        }.map(_.toCompleted)
      case _ => Task.pure(Right(Completed))
    }

  def appointBackupNode(activeUri: Uri, backupUri: Uri): Task[Checked[Completed]] =
    clusterConf.maybeOwnUri match {
      case None =>
        Task.pure(Left(Problem.pure("Missing this node's own URI")))
      case Some(ownUri) =>
        if (backupUri == ownUri)
          Task.pure(Left(Problem.pure("A cluster node can not be appointed to itself")))
        else
          persist {
            case state @ (Empty | _: Sole /*| AwaitingAppointment(Seq(`ownUri`, `backupUri`))*/)
              if activeUri == ownUri =>
                for (events <- becomeSoleIfEmpty(state)) yield
                  events ::: BackupNodeAppointed(backupUri) ::
                    (state match {
                      //case AwaitingAppointment(Seq(`ownUri`, `backupUri`)) => Coupled :: Nil
                      case _ => Nil
                    })
            case state =>
              Left(Problem(s"A backup node can not be appointed while cluster is in state $state"))
          }.map(_.map { case (stampedEvents, state) =>
            proceed(state, stampedEvents.last.eventId)
            Completed
          })
    }

  def passiveNodeFollows(followedUri: Uri, followingUri: Uri): Task[Checked[Completed]] = {
    lazy val call = s"passiveNodeFollows(followedUri=$followedUri, followingUri=$followingUri)"
    clusterConf.maybeOwnUri match {
      case None =>
        Task.pure(Left(Problem.pure("Missing this node's own URI")))
      case Some(ownUri) =>
        if (followedUri != ownUri)
          Task.pure(Left(Problem.pure(s"$call but this is '$ownUri''")))
        else
          persistence.waitUntilStarted.flatMap(_ =>
            persist {
              case clusterState: ClusterState.PreparedToBeCoupled
                if clusterState.activeUri == followedUri && clusterState.passiveUri == followingUri =>
                Right(Nil)

              //case state @ (Empty | Sole(`followedUri`) | NodesAreAppointed(Seq(`followedUri`, `followingUri`))) =>
              //  for (events <- becomeSoleIfEmpty(state)) yield
              //    events ::: FollowingStarted(followingUri) ::
              //      (state match {
              //        case NodesAreAppointed(Seq(`followedUri`, `followingUri`)) => PreparedToBeCoupled :: Nil
              //        case _ => Nil
              //      })

              case NodesAreAppointed(Seq(`followedUri`, `followingUri`)) =>
                Right(FollowingStarted(followingUri) :: Nil)

              case state: Decoupled if state.activeUri == followedUri && state.passiveUri == followingUri =>
                Right(FollowingStarted(followingUri) :: Nil)

              case state =>
                Left(Problem.pure(s"$call rejected due to inappropriate cluster state $state"))
            }.map(_.map { case (stampedEvents, state) =>
              for (stamped <- stampedEvents.lastOption) proceed(state, stamped.eventId)
              Completed
            }))
    }
  }

  def couple(activeUri: Uri, passiveUri: Uri): Task[Checked[Completed]] = {
    lazy val call = s"couple(activeUri=$activeUri, passiveUri=$passiveUri)"
    clusterConf.maybeOwnUri match {
      case None =>
        Task.pure(Left(Problem.pure("Missing this node's own URI")))
      case Some(ownUri) =>
        if (activeUri != ownUri)
          Task.pure(Left(Problem.pure(s"$call but this is '$ownUri''")))
        else
          persist {
            case s: ClusterState.IsCoupled
              if s.activeUri == activeUri && s.passiveUri == passiveUri =>
              Right(Nil)

            case s: ClusterState.PreparedToBeCoupled
              if s.activeUri == activeUri && s.passiveUri == passiveUri =>
              Right(Coupled :: Nil)

            case state =>
              Left(Problem.pure(s"$call rejected due to inappropriate cluster state $state"))
          }.map(_.map { case (stampedEvents, state) =>
            for (stamped <- stampedEvents.lastOption) proceed(state, stamped.eventId)
            Completed
          })
    }
  }

  private def becomeSoleIfEmpty(state: ClusterState): Checked[List[BecameSole]] =
    state match {
      case Empty =>
        clusterConf.maybeOwnUri match {
          case None => Left(Problem.pure("This cluster node's own URI is unknown"))
          case Some(ownUri) => Right(BecameSole(ownUri) :: Nil)
        }
      case _ => Right(Nil)
    }

  def inhibitActivation(duration: FiniteDuration): Task[Checked[ClusterState]] =
    activationInhibitor.inhibitActivation(duration)
      .flatMap {
        case Left(problem) => Task.pure(Left(problem))
        case Right(_) =>
          currentClusterState.map { clusterState =>
            logger.debug(s"inhibitActivation(${duration.pretty}) => $clusterState")
            Right(clusterState)
          }
      }

  def inhibitActivationOfPeer: Task[ClusterState] =
    Task {
      clusterConf.maybeOwnUri.getOrElse(
        throw new IllegalStateException("inhibitActivationOfPeer: ownUri is missing"))
    } .flatMap(ownUri =>
        _currentClusterState.flatMap {
          case clusterState: ClusterState.HasBackupNode if clusterState.isActive(ownUri) =>
            inhibitActivationOf(clusterState.passiveUri)

          case clusterState => Task.raiseError(new IllegalStateException(
            s"inhibitActivationOfPeer expects active cluster node, but clusterState=$clusterState"))
        })

  private def inhibitActivationOf(uri: Uri): Task[ClusterState] =
    Task.defer {
      val retryDelay = 5.s  // TODO
      val retryDelays = Iterator.single(1.s/*TODO*/) ++ Iterator.continually(retryDelay)
      AkkaHttpMasterApi.resource(uri, name = "ClusterInhibitActivation")
        .use(otherNode =>
          otherNode.loginUntilReachable(clusterConf.userAndPassword, retryDelays) >>
            otherNode.executeCommand(MasterCommand.ClusterInhibitActivation(2 * clusterConf.failAfter/*TODO*/))
              .map(_.clusterState))
        .onErrorRestartLoop(()) { (throwable, _, retry) =>
          // TODO Code mit loginUntilReachable usw. zusammenfassen. Stacktrace unterdrücken wenn isNotIgnorableStackTrace
          val msg = "While trying to reach the other cluster node due to restart: " + throwable.toStringWithCauses
          logger.warn(msg)
          for (t <- throwable.ifNoStackTrace) logger.debug(msg, t)
          retry(()).delayExecution(retryDelay)
        }
    }.map { clusterState =>
      logger.debug(s"$uri ClusterInhibitActivation returned clusterState=$clusterState")
      clusterState
    }

  def switchOver: Task[Checked[Completed]] =
    clusterWatchSynchronizer.stopHeartbeats() >>
    persist {
      case coupled: IsCoupled if clusterConf.maybeOwnUri contains coupled.activeUri =>
        Right(SwitchedOver(coupled.passiveUri) :: Nil)
      case state =>
        Left(Problem.pure(s"Switchover is possible only in cluster state IsCoupled(active=${clusterConf.maybeOwnUri.map(_.toString)})," +
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
      case state: IsCoupled =>
        if (clusterConf.maybeOwnUri contains state.activeUri) {
          fetchAndHandleAcknowledgedEventIds(state, eventId)
        } else if (clusterConf.maybeOwnUri contains state.passiveUri) {
          throw new NotImplementedError("Restart of passive cluster node is not yet implemented")
          // FIXME PassiveClusterNode starten
        } else
          sys.error(s"Invalid ClusterState $state")
      case _ =>
    }

  private def fetchAndHandleAcknowledgedEventIds(state: ClusterState.IsCoupled, eventId: EventId): Unit =
    if (!fetchingAcks.getAndSet(true)) {
      def msg = s"observeEventIds(${state.passiveUri}, after=$eventId, userAndPassword=${clusterConf.userAndPassword})"
      fetchingAcksFuture = fetchAndHandleAcknowledgedEventIds(state.passiveUri, after = eventId)
        .executeWithOptions(_.enableAutoCancelableRunLoops)
        .runToFuture
      fetchingAcksFuture onComplete {
        case Success(Left(MissingPassiveClusterNodeHeartbeatProblem(uri))) if clusterConf.maybeOwnUri.isDefined =>
          for (ownUri <- clusterConf.maybeOwnUri) {
            logger.warn("No heartbeat of passive cluster node - continuing as single active cluster node")
            val followerLost = FollowerLost(uri)
            val common = new ClusterCommon(ownUri, activationInhibitor, clusterWatch, testEventBus)
            common.ifClusterWatchAllowsActivation(state, followerLost,
              persist(_ => Right(followerLost :: Nil))
                .map(_.map(_ => true))
            ) .guarantee(Task {
                // FIXME Possible race condition: when immediately Coupled again the acknowledgement are not being read
                //  Mit LockResource synchronisieren?
                for (o <- Option(fetchingAcksFuture)) o.cancel()
                fetchingAcks := false
              })
              .runToFuture
              .onComplete {
                case Failure(t) => logger.error(s"$followerLost event failed: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
                case Success(Left(problem)) => logger.error(s"$followerLost event failed: $problem")
                case Success(Right(false)) => logger.error(s"$followerLost inhibited")  // ???
                case Success(Right(true)) =>
              }
          }

        case tried =>
          tried match {
            case Success(Right(Completed)) =>
              if (!stopRequested) {
                logger.error("observeEventIds terminated")
              }

            case Success(Left(problem)) =>
              logger.error(s"$msg failed with $problem")

            case Failure(t) =>
              logger.error(s"$msg failed with ${t.toStringWithCauses}", t)
          }
          fetchingEventsPromise.complete(tried)
      }
    }

  private def fetchAndHandleAcknowledgedEventIds(uri: Uri, after: EventId): Task[Checked[Completed]] =
    Task { logger.info(s"Fetching acknowledgements from passive cluster node, after=${EventId.toString(after)}") } >>
      AkkaHttpMasterApi.resource(uri, name = "acknowledgements")(actorSystem)
        .use(api =>
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
                  (journalActor ? JournalActor.Input.FollowerAcknowledged(eventId = eventId)).mapTo[Completed]
                    .map(Right.apply)
                }
            }
            .collect {
              case Left(problem) => problem
              //case Right(Completed) => (ignore)
            }
            .headOptionL
            .map {
              case Some(problem) => Left(problem)
              case None => Right(Completed)
            })
        .guaranteeCase(exitCase => Task(
          logger.debug(s"fetchAndHandleAcknowledgedEventIds finished => $exitCase")))

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

  def onClusterCompleted: Task[Checked[Completed]] =
    Task.fromFuture(fetchingEventsPromise.future)

  private def persist(toEvents: ClusterState => Checked[Seq[ClusterEvent]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[ClusterEvent]]], ClusterState)]] =
    (for {
      persisted <- EitherT(persistence.persistTransaction(NoKey)(toEvents))
      (stampedEvents, clusterState) = persisted
      _ <- EitherT(
        if (stampedEvents.isEmpty)
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
      clusterConf.maybeOwnUri match {
        case None => Task.pure(Checked(Completed))
        case Some(ownUri) =>
          if (events.lengthCompare(1) == 0 && events.head.isInstanceOf[SwitchedOver])
            Task.pure(Checked(Completed))  // FailedOver event is reported by the newly active node
          else
            lock.use(_ =>
              Task {
                stopped = true
                heartbeatSender := Cancelable.empty
              } >>
                clusterWatch.applyEvents(from = ownUri, events, clusterState)
            .map(_.map { completed =>
              scheduleHeartbeats(clusterState)
              completed
            }))
      }

    def scheduleHeartbeats(clusterState: ClusterState): Unit = {
      stopped = false
      heartbeatSender := ((clusterConf.maybeOwnUri, clusterState) match {
        case (Some(ownUri), clusterState: HasBackupNode) if clusterState.isActive(ownUri) =>
          scheduler.scheduleAtFixedRate(Duration.Zero, clusterConf.heartbeat) {
            lock.use(_ =>
              currentClusterState.flatMap {
                case clusterState: ClusterState.HasBackupNode if clusterState.isActive(ownUri) && !stopped =>  // check again
                  clusterWatch.heartbeat(from = ownUri, clusterState)
                case _ =>
                  Task.pure(Right(Completed))
              }
            ) .runToFuture onComplete {
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

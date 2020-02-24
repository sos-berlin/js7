package com.sos.jobscheduler.master.cluster

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.http.{AkkaHttpClient, RecouplingStreamReader}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.core.event.journal.recover.{JournaledStateRecoverer, Recovered}
import com.sos.jobscheduler.core.event.state.JournaledStatePersistence
import com.sos.jobscheduler.core.problems.MissingPassiveClusterNodeHeartbeatProblem
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, BecameSole, ClusterCoupled, FollowerLost, FollowingStarted, SwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterState.{AwaitingAppointment, AwaitingFollower, Coupled, CoupledOrDecoupled, Decoupled, Empty, OtherFailedOver, Sole}
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterNodeRole, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, Stamped}
import com.sos.jobscheduler.master.MasterState
import com.sos.jobscheduler.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import com.sos.jobscheduler.master.cluster.Cluster._
import com.sos.jobscheduler.master.cluster.ObservablePauseDetector._
import com.typesafe.config.Config
import java.io.RandomAccessFile
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Paths}
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.{Observable, OverflowStrategy}
import scala.concurrent.Promise
import scala.util.{Failure, Success}

final class Cluster(
  journalMeta: JournalMeta,
  persistence: JournaledStatePersistence[ClusterState, ClusterEvent],
  clusterConf: ClusterConf,
  config: Config,
  eventIdGenerator: EventIdGenerator,
  actorSystem: ActorSystem)
  (implicit s: Scheduler, journalActorAskTimeout: Timeout)
{
  private val journalActor = persistence.journalActor
  private val fetchingEvents = AtomicBoolean(false)
  @volatile
  private var fetchingEventsFuture: CancelableFuture[Checked[Completed]] = null
  private val fetchingEventsPromise = Promise[Checked[Completed]]()
  @volatile
  private var switchoverAcknowledged = false
  @volatile
  private var stopRequested = false
  private val started = Promise[Unit]()
  @volatile
  private var startingClusterState: Option[ClusterState] = null

  def stop(): Unit = {
    logger.debug("stop")
    stopRequested = true
    for (o <- Option(fetchingEventsFuture)) {
      o.cancel()
    }
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
    startingClusterState = Some(recoveredClusterState)
    started.success(())
    val (passiveState, followUp) = startCluster(recovered, recoveredClusterState, recoveredState, eventIdGenerator)
    passiveState ->
      followUp.map(_.map { case (clusterState, followUp) =>
        startingClusterState = None
        // clusterWatch.heartbeat(ownUri, persistence.currentState)
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
        logger.info(s"Becoming the active primary cluster node, still without backup")
        Task.pure(None) ->
          Task.pure(Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered)))

      case (Empty, Some(ownUri), Some(ClusterNodeRole.Backup(activeUri)), _) =>
        logger.info(s"Becoming the (still not following) backup cluster node '$ownUri' for primary node at $activeUri")
        val passive = new PassiveClusterNode(ownUri, activeUri, journalMeta, recovered, clusterConf, eventIdGenerator)(actorSystem)
        passive.state.map(Some.apply) ->
          passive.run(recoveredClusterState, recoveredState)

      case (_, Some(ownUri), _, Some(recoveredJournalFile)) if recoveredClusterState.isActive(ownUri) =>
        recoveredClusterState match {
          case recoveredClusterState: Coupled =>
            val retryDelay = 5.s  // TODO
            val retryDelays = Iterator.single(1.s/*TODO*/) ++ Iterator.continually(retryDelay)
            val failedOver = AkkaHttpMasterApi.resource(recoveredClusterState.passiveUri, name = "clusterState")(actorSystem)
              .use(other =>
                other.loginUntilReachable(clusterConf.userAndPassword, retryDelays) >>
                  other.clusterState)
              .onErrorRestartLoop(()) { (throwable, _, retry) =>
                logger.warn("While trying to reach the other cluster node due to restart: " + throwable.toStringWithCauses,
                  throwable.nullIfNoStackTrace)
                retry(()).delayExecution(retryDelay)
              }
              .map {
                case otherState: ClusterState if otherState.isActive(ownUri) =>
                  // FIXME Race condition: Wie sicherstellen, dass bisher Passiver sich nicht gerade jetzt aktiviert?
                  // Vielleicht eine Mindestzeit für einen Neustart abwarten.
                  logger.info(s"Remaining the active cluster node with '${recoveredClusterState.passiveUri}' passive node following")
                  assertThat(recoveredClusterState.isActive(ownUri))
                  proceed(recoveredClusterState, recovered.eventId)
                  None

                case otherState: CoupledOrDecoupled =>
                  logger.info(s"The other cluster node '${otherState.activeUri}' has failed-over while this node was absent")
                  val failedAt = otherState.failedAt getOrElse sys.error("Missing failedAt information from failed-over cluster node")
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
                      logger.info(s"Removing journal file '${recoveredJournalFile.file}'")
                      truncated = true
                      val file = journalFiles.last.file
                      Files.move(file, Paths.get(file + "~"), REPLACE_EXISTING)
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
                    logger.info(s"Truncating journalFile ${journalFile.file.getFileName} at failed-over position")
                    val f = new RandomAccessFile(file.toFile, "rw")
                    try {
                      f.seek(failedAt.position - 1)
                      if (f.read() != '\n')
                        sys.error(s"Invalid failed-over position=${failedAt.position} in '${journalFile.file.getFileName} journal file")
                      //TODO assertThat(f.readLineReverse.as[Json]("eventId") == failedAt.eventId)
                      f.setLength(failedAt.position)
                    } finally f.close()
                  }
                  val truncatedRecoveredJournalFile = if (!truncated) recoveredJournalFile else
                    JournaledStateRecoverer.recover[MasterState, Event](journalMeta, recovered.newStateBuilder, config /*, runningSince=???*/)
                      .recoveredJournalFile getOrElse sys.error(s"Unrecoverable journal file '${file.getFileName}''")
                  assertThat(truncatedRecoveredJournalFile.state.clusterState == recoveredClusterState)
                  val passiveClusterNode = new PassiveClusterNode(ownUri, otherState.activeUri, journalMeta,
                    recovered.copy(recoveredJournalFile = Some(
                      truncatedRecoveredJournalFile.copy[MasterState, Event](
                        state = truncatedRecoveredJournalFile.state.copy(
                          clusterState = ClusterState.OtherFailedOver(
                            recoveredClusterState.passiveUri,
                            recoveredClusterState.activeUri))))),
                    clusterConf, eventIdGenerator)(actorSystem)
                  Some(OtherFailedOver(otherState.activeUri, otherState.passiveUri) -> passiveClusterNode)
                case otherState =>
                  sys.error(s"The other node is in invalid clusterState=$otherState")  // ???
              }
              .memoize
            failedOver.flatMap {
              case None => Task.pure(None)
              case Some((_, passiveClusterNode)) => passiveClusterNode.state map Some.apply
            } ->
             failedOver.flatMap {
               case None => Task.pure(Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered)))
               case Some((otherClusterState, passiveClusterNode)) => passiveClusterNode.run(otherClusterState, recoveredState)
             }

          case _ =>
            logger.info("Becoming the active cluster node without following node")
            proceed(recoveredClusterState, recovered.eventId)
            Task.pure(None) ->
              Task.pure(Right(recoveredClusterState -> ClusterFollowUp.BecomeActive(recovered)))
        }

      case (state: CoupledOrDecoupled, Some(ownUri), _, Some(_)) if state.passiveUri == ownUri =>
        logger.info(
          if (state.isTheFollowingNode(ownUri))
            s"Becoming a following passive cluster node '$ownUri'"
          else
            s"Becoming a passive cluster node '$ownUri', trying to follow the active node")
        val passive = new PassiveClusterNode(ownUri, state.activeUri, journalMeta, recovered, clusterConf, eventIdGenerator)(actorSystem)
        passive.state.map(Some.apply) ->
          passive.run(recoveredClusterState, recoveredState)

      case (_, None, _, _) =>
        Task.pure(None) ->
          Task.pure(Left(Problem.pure(s"This cluster node's own URI is unknown, clusterState=$recoveredClusterState")))

      case (_, Some(ownUri), _, _) =>
        Task.pure(None) ->
          Task.pure(Left(Problem.pure(s"This cluster node's URI '$ownUri' does not match state $recoveredClusterState")))
    }

  def automaticallyAppointConfiguredBackupNode(): Task[Checked[Completed]] =
    clusterConf.maybeRole match {
      case Some(ClusterNodeRole.Primary(Some(backupUri))) =>
        persistence.persistTransaction(NoKey) {
          case state @ (ClusterState.Empty | _: ClusterState.Sole | _: ClusterState.AwaitingAppointment) =>
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
          persistence.persistTransaction(NoKey) {
            case state @ (Empty | _: Sole | AwaitingAppointment(`ownUri`, `backupUri`) /*| AwaitingFollower(`ownUri`, `backupUri`)*/)
              if activeUri == ownUri =>
                for (events <- becomeSoleIfEmpty(state)) yield
                  events ::: BackupNodeAppointed(backupUri) ::
                    (state match {
                      case AwaitingAppointment(`ownUri`, `backupUri`) => ClusterCoupled :: Nil
                      case _ => Nil
                    })
            case state =>
              Left(Problem(s"A backup node can not be appointed while cluster is in state $state"))
          }.map(_.map { case (stampedEvents, state) =>
            proceed(state, stampedEvents.last.eventId)
            Completed
          })
    }

  def passiveNodesFollows(followedUri: Uri, followingUri: Uri): Task[Checked[Completed]] =
    clusterConf.maybeOwnUri match {
      case None =>
        Task.pure(Left(Problem.pure("Missing this node's own URI")))
      case Some(ownUri) =>
        persistence.waitUntilStarted.flatMap(_
          .persistTransaction(NoKey) {
            case _: Coupled =>
              Right(Nil)
            case state @ (Empty | Sole(`ownUri`) | AwaitingFollower(`ownUri`, `followingUri`)) if ownUri == followedUri =>
              for (events <- becomeSoleIfEmpty(state)) yield
                events ::: FollowingStarted(followingUri) ::
                  (state match {
                    case AwaitingFollower(`ownUri`, `followingUri`) => ClusterCoupled :: Nil
                    case _ => Nil
                  })
            case AwaitingFollower(`ownUri`, `followingUri`) | Decoupled(`ownUri`, `followingUri`, _) if ownUri == followedUri =>
              Right(FollowingStarted(followingUri) :: ClusterCoupled :: Nil)
            case state =>
              Left(Problem.pure(s"Following cluster node '$followingUri' ignored due to inappropriate cluster state $state"))
          }.map(_.map { case (stampedEvents, state) =>
            for (stamped <- stampedEvents.lastOption) proceed(state, stamped.eventId)
            Completed
          }))
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

  def switchOver: Task[Checked[Completed]] =
    persistence.persistEvent[ClusterEvent](NoKey) {
      case coupled: Coupled if clusterConf.maybeOwnUri contains coupled.activeUri =>
        Right(SwitchedOver(coupled.passiveUri))
      case state =>
        Left(Problem.pure(s"Switchover is possible only in cluster state Coupled(active=${clusterConf.maybeOwnUri.map(_.toString)})," +
          s" but cluster state is: $state"))
    }.map(_.map { case (_: Stamped[_], _) =>
      switchoverAcknowledged = true
      Completed
    })

  private def proceed(state: ClusterState, eventId: EventId): Unit =
    state match {
      case state: Coupled =>
        if (clusterConf.maybeOwnUri contains state.activeUri) {
          proceedAsActive(state, eventId)
        } else if (clusterConf.maybeOwnUri contains state.passiveUri) {
          throw new NotImplementedError("Restart of passive cluster node is not yet implemented")
          // FIXME PassiveClusterNode starten
        } else
          sys.error(s"Invalid ClusterState $state")
      case _ =>
    }

  private def proceedAsActive(state: ClusterState.Coupled, eventId: EventId): Unit = {
    if (fetchingEvents.getAndSet(true)) throw new AssertionError("Already fetching events?")
    def msg = s"observeEventIds(${state.passiveUri}, after=$eventId, userAndPassword=${clusterConf.userAndPassword})"
    fetchingEventsFuture = fetchAndHandleAcknowledgedEventIds(state.passiveUri, after = eventId)
      .executeWithOptions(_.enableAutoCancelableRunLoops)
      .runToFuture
    fetchingEventsFuture onComplete {
      case Success(Left(MissingPassiveClusterNodeHeartbeatProblem(uri))) =>
        logger.warn("Missing heartbeat of passive cluster node - continuing as a single node")
        val event = FollowerLost(uri)
        persistence.persistEvent[ClusterEvent](NoKey)(_ => Right(event))
          .runToFuture
          .onComplete {
            case Success(Right(_)) => logger.info(s"Continuing as a single cluster node after passive node failed")
            case failed => logger.error(s"$event event failed: $failed")
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
    Task { logger.info(s"Fetching acknowledged EventIds from passive cluster node, after=${EventId.toString(after)}") } >>
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

  /*
  private def queryAgents(otherNodeId: ClusterNodeId): Unit =
    Task
      .sequence(
        for (a <- votingAgentRefPaths.toVector) yield
          askAgent(a)
            .recover { case throwable =>
              logger.error(throwable.toStringWithCauses)  // We ignore the error
              Set.empty
            })
      .map(_.flatten)
      // TODO Timeout einbauen
      .runToFuture onComplete {
        case Failure(throwable) =>
          logger.error(throwable.toStringWithCauses)  // Should not happen because we have recovered already
        case Success(nodeIds) =>
          val me = nodeIds.count(_ == conf.uri)
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

  /** Returns the current or at start the recovered ClusterState.
    * Required for the /api/cluster web service used by the restarting active node
    * asking the peer about its current (maybe failed-over) ClusterState. */
  def currentClusterState: Task[ClusterState] =
    Task.fromFuture(started.future).flatMap(_ =>
      startingClusterState match {
        case Some(o) => Task.pure(o)  // Use recovered (maybe old) ClusterState until the actual ClusterState has been established
        case None => persistence.currentState
      })

  // Used to RegisterMe actor in JournalActor
  def journalingActor = persistence.actor
}

object Cluster
{
  private val logger = Logger(getClass)
}

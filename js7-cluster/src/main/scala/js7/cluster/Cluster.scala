package js7.cluster

import akka.util.Timeout
import cats.effect.Resource
import com.softwaremill.diffx
import com.typesafe.config.Config
import izumi.reflect.Tag
import java.nio.file.Path
import js7.base.eventbus.EventPublisher
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SetOnce
import js7.base.web.Uri
import js7.cluster.Cluster.*
import js7.cluster.JournalTruncator.truncateJournal
import js7.core.license.LicenseChecker
import js7.data.Problems.{BackupClusterNodeNotAppointed, ClusterNodeIsNotBackupProblem, PrimaryClusterNodeMayNotBecomeBackupProblem}
import js7.data.cluster.ClusterCommand.{ClusterInhibitActivation, ClusterStartBackupNode}
import js7.data.cluster.ClusterState.{Coupled, Empty, FailedOver, HasNodes}
import js7.data.cluster.ClusterWatchCommand.ClusterWatchAcknowledge
import js7.data.cluster.{ClusterCommand, ClusterNodeApi, ClusterSetting, ClusterWatchCommand, ClusterWatchMessage}
import js7.data.event.{EventId, JournalPosition, SnapshotableState}
import js7.journal.EventIdGenerator
import js7.journal.data.JournalMeta
import js7.journal.recover.{Recovered, StateRecoverer}
import js7.journal.state.FileStatePersistence
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.util.control.NoStackTrace

final class Cluster[S <: SnapshotableState[S]: diffx.Diff: Tag] private(
  persistence: FileStatePersistence[S],
  journalMeta: JournalMeta,
  val clusterConf: ClusterConf,
  eventIdGenerator: EventIdGenerator,
  common: ClusterCommon)
  (implicit
    S: SnapshotableState.Companion[S],
    scheduler: Scheduler)
{
  import clusterConf.ownId
  import common.{activationInhibitor, config}

  private val keepTruncatedRest = config.getBoolean("js7.journal.cluster.keep-truncated-rest")
  @volatile private var _passiveOrWorkingNode: Option[Either[PassiveClusterNode[S], WorkingClusterNode[S]]] = None
  private val expectingStartBackupCommand = SetOnce[Promise[ClusterStartBackupNode]]

  def stop: Task[Completed] =
    Task.defer {
      if (isActive || isPassive) logger.info("Stop cluster node")
      else logger.debug("Stop cluster node")
      (_passiveOrWorkingNode match {
        case Some(Left(passiveClusterNode)) => passiveClusterNode.onShutDown
        case Some(Right(workingClusterNode)) => workingClusterNode.stop
        case _ => Task.unit
      }) >>
        common.stop
    }

  /**
    * Returns a pair of `Task`s
    * - `(Task[None], _)` no replicated state available, because it's an active node or the backup node has not yet been appointed.
    * - `(Task[Some[Checked[S]]], _)` with the current replicated state if this node has started as a passive node.
    * - `(_, Task[Checked[ClusterFollowUp]]` when this node should be activated
    * @return A pair of `Task`s with maybe the current `S` of this passive node (if so)
    *         and ClusterFollowUp.
    */
  def start(recovered: Recovered[S]): (Task[Option[Checked[S]]], Task[Checked[Recovered[S]]]) = {
    if (recovered.clusterState != Empty) logger.info(
      s"This cluster node '${ownId.string}', recovered ClusterState is ${recovered.clusterState}")
    val (currentPassiveReplicatedState, activeRecovered) = startNode(recovered)
    currentPassiveReplicatedState ->
      activeRecovered.flatMapT(startWorkingNode)
  }

  private def startWorkingNode(recovered: Recovered[S]): Task[Checked[Recovered[S]]] = {
    val workingClusterNode = new WorkingClusterNode(persistence, common, clusterConf)
    assertThat(!_passiveOrWorkingNode.exists(_.isRight))
    _passiveOrWorkingNode = Some(Right(workingClusterNode))
    workingClusterNode.startIfNonEmpty(recovered.clusterState, recovered.eventId)
      .map(_.map((_: Completed) => recovered))
  }

  private def startNode(recovered: Recovered[S])
  : (Task[Option[Checked[S]]], Task[Checked[Recovered[S]]]) =
    recovered.clusterState match {
      case Empty =>
        if (clusterConf.isPrimary) {
          logger.debug(s"Active primary cluster $ownId, no backup node appointed")
          Task.pure(None) ->
            (activationInhibitor.startActive >>
              Task.pure(Right(recovered)))
        } else if (recovered.eventId != EventId.BeforeFirst)
          Task.pure(Some(Left(PrimaryClusterNodeMayNotBecomeBackupProblem))) ->
            Task.pure(Left(PrimaryClusterNodeMayNotBecomeBackupProblem))
        else
          startAsBackupNodeWithEmptyClusterState(recovered)

      case clusterState: HasNodes =>
        if (ownId == clusterState.activeId)
          startAsActiveNodeWithBackup(recovered)
        else if (ownId == clusterState.passiveId)
          startAsPassiveNode(recovered, clusterState)
        else
          Task.pure(None) ->
            Task.pure(Left(Problem.pure(s"Own cluster $ownId " +
              s"does not match clusterState=${recovered.clusterState}")))
    }

  private def startAsBackupNodeWithEmptyClusterState(recovered: Recovered[S])
  : (Task[Option[Checked[S]]], Task[Checked[Recovered[S]]]) = {
    logger.info(s"Backup cluster $ownId, awaiting appointment from a primary node")
    val startedPromise = Promise[ClusterStartBackupNode]()
    expectingStartBackupCommand := startedPromise
    val passiveClusterNode = Task.deferFuture(startedPromise.future)
      .map(cmd =>
        newPassiveClusterNode(recovered, cmd.setting, initialFileEventId = Some(cmd.fileEventId)))
      .memoize
    val passiveState = Task.defer {
      if (startedPromise.future.isCompleted)
        passiveClusterNode.flatMap(_.state.map(s => Some(Right(s))))
      else
        Task.pure(Some(Left(BackupClusterNodeNotAppointed)))
    }
    val followUp = passiveClusterNode.flatMap(passive =>
      activationInhibitor.startPassive >>
        passive.run(recovered.state))
    passiveState -> followUp
  }

  private def startAsActiveNodeWithBackup(recovered: Recovered[S])
  : (Task[Option[Checked[S]]], Task[Checked[Recovered[S]]]) = {
    recovered.clusterState match {
      case recoveredClusterState: Coupled =>
        import recoveredClusterState.passiveId
        logger.info(s"This cluster $ownId was active and coupled before restart - " +
          s"asking '${passiveId.string}' node about its state")
        val failedOver = common.inhibitActivationOfPeer(recoveredClusterState).map {
          case None/*Other node has not failed-over*/ =>
            logger.info(s"The other '${passiveId.string}' cluster node is up and still passive, " +
              "so this node remains the active cluster node")
            None

          case Some(otherFailedOver) =>
            Some(startPassiveAfterFailover(recovered, recoveredClusterState, otherFailedOver))
        }.memoize

        failedOver.flatMap {
          case None => Task.pure(None)
          case Some((_, passiveClusterNode)) => passiveClusterNode.state.map(s => Some(Right(s)))
        } ->
          failedOver.flatMap {
            case None =>
              Task.pure(Right(recovered))
            case Some((ourRecovered, passiveClusterNode)) =>
              passiveClusterNode.run(ourRecovered.state)
          }

      case _ =>
        logger.info("Remaining the active cluster node, not coupled with passive node")
        Task.pure(None) ->
          (activationInhibitor.startActive >>
            Task.pure(Right(recovered)))
    }
  }

  def startPassiveAfterFailover(recovered: Recovered[S], coupled: Coupled, otherFailedOver: FailedOver)
  : (Recovered[S], PassiveClusterNode[S]) = {
    logger.warn(s"The other $otherFailedOver.activeId} failed-over and " +
      "became active while this node was absent")
    assertThat(otherFailedOver.idToUri == coupled.idToUri &&
               otherFailedOver.activeId == coupled.passiveId)
    // This restarted, previously failed active cluster node may have written one chunk of events more than
    // the passive node, maybe even an extra snapshot in a new journal file.
    // These extra events are not acknowledged. So we truncate our journal.
    val ourRecovered = truncateJournalAndRecoverAgain(otherFailedOver) match {
      case None => recovered
      case Some(truncatedRecovered) =>
        assertThat(truncatedRecovered.state.clusterState == coupled)
        assertThat(!recovered.eventWatch.whenStarted.isCompleted)
        recovered.close()  // Should do nothing, because recovered.eventWatch has not been started
        truncatedRecovered
    }
    ourRecovered -> newPassiveClusterNode(ourRecovered, otherFailedOver.setting, otherFailedOver = true)
  }

  private def truncateJournalAndRecoverAgain(otherFailedOver: FailedOver): Option[Recovered[S]] =
    for (file <- truncateJournal(journalMeta.fileBase, otherFailedOver.failedAt, keepTruncatedRest))
      yield recoverFromTruncated(file, otherFailedOver.failedAt)

  private def recoverFromTruncated(file: Path, failedAt: JournalPosition): Recovered[S] = {
    logger.info("Recovering again after unacknowledged events have been deleted properly from journal file")
    throw new RestartAfterJournalTruncationException

    // May take a long time !!!
    val recovered = StateRecoverer.recover[S](journalMeta, config)

    // Assertions
    val recoveredJournalFile = recovered.recoveredJournalFile
      .getOrElse(sys.error(s"Unrecoverable journal file: ${file.getFileName}"))
    assertThat(recoveredJournalFile.file == file)
    assertThat(recoveredJournalFile.journalPosition == failedAt,
      s"${recoveredJournalFile.journalPosition} != $failedAt")

    recovered
  }

  private def startAsPassiveNode(recovered: Recovered[S], clusterState: HasNodes) = {
    logger.info(
      if (clusterState.isInstanceOf[Coupled])
        s"Remaining a passive cluster node following the active ${clusterState.activeId}"
      else
        s"Remaining a passive cluster node trying to follow the active ${clusterState.activeId}")
    val passive = newPassiveClusterNode(recovered, clusterState.setting)
    passive.state.map(s => Some(Right(s))) ->
      passive.run(recovered.state)
  }

  private def newPassiveClusterNode(
    recovered: Recovered[S],
    setting: ClusterSetting,
    otherFailedOver: Boolean = false,
    initialFileEventId: Option[EventId] = None)
  : PassiveClusterNode[S] = {
    assertThat(!_passiveOrWorkingNode.exists(_.isLeft))
    val node = new PassiveClusterNode(ownId, setting, recovered,
      eventIdGenerator, initialFileEventId,
      otherFailedOver, persistence.journalConf, clusterConf, config, common)
    _passiveOrWorkingNode = Some(Left(node))
    node
  }

  def workingClusterNode: Checked[WorkingClusterNode[S]] =
    _passiveOrWorkingNode
      .flatMap(_.toOption)
      .toRight(Problem.pure(s"This cluster $ownId is not active"))

  def executeCommand(command: ClusterCommand): Task[Checked[ClusterCommand.Response]] =
    command match {
      case command: ClusterCommand.ClusterStartBackupNode =>
        Task {
          expectingStartBackupCommand.toOption match {
            case None =>
              if (!clusterConf.isBackup)
                Left(ClusterNodeIsNotBackupProblem)
              else
                Left(Problem.pure("Cluster node is not ready to accept a backup node configuration"))
            case Some(promise) =>
              if (command.setting.passiveId != ownId)
                Left(Problem.pure(s"$command sent to wrong $ownId"))
              else if (command.setting.activeId == ownId)
                Left(Problem.pure(s"$command must not be sent to the active node"))
              else {
                promise.trySuccess(command)
                Right(ClusterCommand.Response.Accepted)
              }
          }
        }

      case ClusterCommand.ClusterInhibitActivation(duration) =>
        activationInhibitor.inhibitActivation(duration)
          .flatMapT(inhibited =>
            if (inhibited)
              Task.pure(Right(ClusterInhibitActivation.Response(None)))
            else
              // Could not inhibit, so this node is already active.
              // awaitCurrentState will return (maybe almost?) immediately.
              persistence.awaitCurrentState
                .map(_.clusterState)
                .map(Some(_))
                .timeoutTo(duration/*???*/ - 500.ms, Task.none)
                .flatMap {
                  case None =>
                    // No persistence
                    Task.left(Problem.pure(
                      "ClusterInhibitActivation command failed — please try again"))

                  case Some(failedOver: FailedOver) =>
                    logger.debug(s"inhibitActivation(${duration.pretty}) => $failedOver")
                    Task.right(ClusterInhibitActivation.Response(Some(failedOver)))

                  case Some(clusterState) =>
                    Task.left(Problem.pure("ClusterInhibitActivation command failed " +
                      s"because node is already active but not failed-over: $clusterState"))
                })

      case _: ClusterCommand.ClusterPrepareCoupling |
           _: ClusterCommand.ClusterCouple |
           _: ClusterCommand.ClusterRecouple |
           _: ClusterCommand.ClusterPassiveDown =>
        Task.pure(workingClusterNode)
          .flatMapT(_.executeCommand(command))
    }

  def executeClusterWatchCommand(command: ClusterWatchCommand): Task[Checked[Unit]] =
    command match {
      case ClusterWatchAcknowledge(requestId, maybeProblem) =>
        Task(common
          .maybeClusterWatchSynchronizer
          .toRight(Problem(s"No ClusterWatchSynchronizer: $command"))
          .flatMap(o => o
            .clusterWatch.onClusterWatchAcknowledged(requestId, maybeProblem))
          .map(_ => ClusterCommand.Response.Accepted))
    }

  def clusterWatchMessageStream: Task[fs2.Stream[Task, ClusterWatchMessage]] =
    common.clusterWatch.newStream

  /** Is the active or non-cluster (Empty, isPrimary) node or is becoming active. */
  def isWorkingNode = _passiveOrWorkingNode.exists(_.isRight)

  /** Is active with a backup node. */
  def isActive = _passiveOrWorkingNode.exists(_.exists(_.isActive))

  def isPassive = _passiveOrWorkingNode.exists(_.isLeft)
}

object Cluster
{
  private val logger = Logger(getClass)

  def apply[S <: SnapshotableState[S] : diffx.Diff : Tag](
    persistence: FileStatePersistence[S],
    journalMeta: JournalMeta,
    clusterConf: ClusterConf,
    config: Config,
    eventIdGenerator: EventIdGenerator,
    clusterNodeApi: (Uri, String) => Resource[Task, ClusterNodeApi],
    licenseChecker: LicenseChecker,
    testEventPublisher: EventPublisher[Any],
    journalActorAskTimeout: Timeout)
    (implicit
      S: SnapshotableState.Companion[S],
      scheduler: Scheduler)
  : Cluster[S] =
    new Cluster(
      persistence,
      journalMeta,
      clusterConf,
      eventIdGenerator,
      new ClusterCommon(clusterConf.ownId, clusterNodeApi, clusterConf.timing,
        config, licenseChecker, testEventPublisher, journalActorAskTimeout))

  @deprecated("Provisional fix for v2.4", "v2.5")
  final class RestartAfterJournalTruncationException
  extends RuntimeException("Restart after journal truncation")
  with NoStackTrace
}

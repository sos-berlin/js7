package js7.cluster

import akka.actor.ActorSystem
import akka.util.Timeout
import cats.effect.Resource
import com.softwaremill.diffx
import com.typesafe.config.Config
import izumi.reflect.Tag
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.StandardOpenOption.{CREATE, READ, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Files, Path, Paths}
import js7.base.eventbus.EventPublisher
import js7.base.generic.Completed
import js7.base.io.https.HttpsConfig
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SetOnce
import js7.base.web.Uri
import js7.cluster.Cluster.*
import js7.core.license.LicenseChecker
import js7.data.Problems.{BackupClusterNodeNotAppointed, ClusterNodeIsNotBackupProblem, PrimaryClusterNodeMayNotBecomeBackupProblem}
import js7.data.cluster.ClusterCommand.{ClusterInhibitActivation, ClusterStartBackupNode}
import js7.data.cluster.ClusterState.{Coupled, Empty, FailedOver, HasNodes}
import js7.data.cluster.{ClusterCommand, ClusterNodeApi, ClusterSetting}
import js7.data.controller.ControllerId
import js7.data.event.{EventId, JournalPosition, SnapshotableState}
import js7.journal.EventIdGenerator
import js7.journal.data.JournalMeta
import js7.journal.files.JournalFiles
import js7.journal.recover.{Recovered, StateRecoverer}
import js7.journal.state.FileStatePersistence
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Promise

final class Cluster[S <: SnapshotableState[S]: diffx.Diff: Tag] private(
  persistence: FileStatePersistence[S],
  journalMeta: JournalMeta,
  eventIdGenerator: EventIdGenerator,
  common: ClusterCommon,
  val clusterConf: ClusterConf)
  (implicit
    S: SnapshotableState.Companion[S],
    scheduler: Scheduler)
{
  import clusterConf.ownId
  import common.{activationInhibitor, config}
  import persistence.journalConf
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
      }) *>
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
  def start(recovered: Recovered[S]): (Task[Option[Checked[S]]], Task[Checked[ClusterFollowUp[S]]]) = {
    if (recovered.clusterState != Empty) logger.info(s"Recovered ClusterState is ${recovered.clusterState}")
    val (currentPassiveReplicatedState, followUp) = startNode(recovered)
    val workingFollowUp = followUp.flatMapT {
      case followUp: ClusterFollowUp.BecomeActive[S] =>
        val workingClusterNode = new WorkingClusterNode(persistence, common, clusterConf)
        assertThat(!_passiveOrWorkingNode.exists(_.isRight))
        _passiveOrWorkingNode = Some(Right(workingClusterNode))
        workingClusterNode.startIfNonEmpty(followUp.recovered.clusterState, followUp.recovered.eventId)
          .map(_.map((_: Completed) => followUp))
      }
    currentPassiveReplicatedState -> workingFollowUp
  }

  private def startNode(recovered: Recovered[S])
  : (Task[Option[Checked[S]]], Task[Checked[ClusterFollowUp[S]]]) =
    recovered.clusterState match {
      case Empty =>
        if (clusterConf.isPrimary) {
          logger.debug(s"Active primary cluster $ownId, no backup node appointed")
          Task.pure(None) ->
            (activationInhibitor.startActive *>
              Task.pure(Right(ClusterFollowUp.BecomeActive(recovered))))
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
  : (Task[Option[Checked[S]]], Task[Checked[ClusterFollowUp[S]]]) = {
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
      activationInhibitor.startPassive *>
        passive.run(recovered.state))
    passiveState -> followUp
  }

  private def startAsActiveNodeWithBackup(recovered: Recovered[S])
  : (Task[Option[Checked[S]]], Task[Checked[ClusterFollowUp[S]]]) = {
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
              Task.pure(Right(ClusterFollowUp.BecomeActive(recovered)))
            case Some((ourRecovered, passiveClusterNode)) =>
              passiveClusterNode.run(ourRecovered.state)
          }

      case _ =>
        logger.info("Remaining the active cluster node, not coupled with passive node")
        Task.pure(None) ->
          (activationInhibitor.startActive *>
            Task.pure(Right(ClusterFollowUp.BecomeActive(recovered))))
    }
  }

  def startPassiveAfterFailover(recovered: Recovered[S], coupled: Coupled, otherFailedOver: FailedOver)
  : (Recovered[S], PassiveClusterNode[S]) = {
    logger.warn(s"The other ${otherFailedOver.activeId} failed-over and " +
      s"became active while this node was absent")
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
    for (file <- truncateJournal(journalMeta.fileBase, otherFailedOver.failedAt))
      yield recoverFromTruncated(file, otherFailedOver.failedAt)

  private def recoverFromTruncated(file: Path, failedAt: JournalPosition): Recovered[S] = {
    logger.info("Recovering again after unacknowledged events have been deleted properly from journal file")

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
    val node = new PassiveClusterNode(ownId, setting,
      recovered, journalConf, eventIdGenerator,
      initialFileEventId, otherFailedOver,
      clusterConf, config, common)
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
              persistence.awaitCurrentState.map(_.clusterState).map {
                case failedOver: FailedOver =>
                  logger.debug(s"inhibitActivation(${duration.pretty}) => $failedOver")
                  Right(ClusterInhibitActivation.Response(Some(failedOver)))
                case clusterState =>
                  Left(Problem.pure("ClusterInhibitActivation command failed " +
                    s"because node is already active but not failed-over: $clusterState"))
              })

      case _: ClusterCommand.ClusterPrepareCoupling |
           _: ClusterCommand.ClusterCouple |
           _: ClusterCommand.ClusterRecouple |
           _: ClusterCommand.ClusterPassiveDown =>
        Task.pure(workingClusterNode)
          .flatMapT(_.executeCommand(command))
    }

  /** Is the active or non-cluster (Empty, isPrimary) node or is becoming active. */
  def isWorkingNode = _passiveOrWorkingNode.exists(_.isRight)

  /** Is active with a backup node. */
  def isActive = _passiveOrWorkingNode.exists(_.exists(_.isActive))

  def isPassive = _passiveOrWorkingNode.exists(_.isLeft)

  def manualClusterWatch =
    common.manualClusterWatch
}

object Cluster
{
  private val logger = Logger(getClass)

  def apply[S <: SnapshotableState[S]: diffx.Diff: Tag](
    persistence: FileStatePersistence[S],
    journalMeta: JournalMeta,
    eventIdGenerator: EventIdGenerator,
    clusterNodeApi: (Uri, String) => Resource[Task, ClusterNodeApi],
    clusterConf: ClusterConf,
    controllerId: ControllerId,
    httpsConfig: HttpsConfig,
    config: Config,
    licenseChecker: LicenseChecker,
    testEventPublisher: EventPublisher[Any],
    journalActorAskTimeout: Timeout)
    (implicit
      S: SnapshotableState.Companion[S],
      scheduler: Scheduler,
      actorSystem: ActorSystem)
  : Cluster[S] =
    new Cluster(
      persistence: FileStatePersistence[S],
      journalMeta: JournalMeta,
      eventIdGenerator: EventIdGenerator,
      new ClusterCommon(controllerId, clusterConf.ownId, clusterNodeApi,
        httpsConfig, config, licenseChecker, testEventPublisher, journalActorAskTimeout),
      clusterConf: ClusterConf)

  private[cluster] def truncateJournal(journalFileBase: Path, failedAt: JournalPosition): Option[Path] = {
    var truncated = false
    val lastTwoJournalFiles = JournalFiles.listJournalFiles(journalFileBase) takeRight 2
    val journalFile =
      if (lastTwoJournalFiles.last.fileEventId == failedAt.fileEventId)
        lastTwoJournalFiles.last
      else if (lastTwoJournalFiles.lengthIs == 2 &&
        lastTwoJournalFiles.head.fileEventId == failedAt.fileEventId &&
        lastTwoJournalFiles.last.fileEventId > failedAt.fileEventId)
      {
        truncated = true
        val deleteFile = lastTwoJournalFiles.last.file
        logger.info(s"Removing journal file written after failover: ${deleteFile.getFileName}")
        // Keep the file for debugging
        Files.move(deleteFile, Paths.get(s"$deleteFile~DELETED-AFTER-FAILOVER"), REPLACE_EXISTING)
        JournalFiles.updateSymbolicLink(journalFileBase, lastTwoJournalFiles.head.file)
        lastTwoJournalFiles.head
      } else
        sys.error(s"Failed-over node's JournalPosition does not match local journal files:" +
          s" $failedAt <-> ${lastTwoJournalFiles.map(_.file.getFileName).mkString(", ")}")
    assertThat(journalFile.fileEventId == failedAt.fileEventId)

    val file = journalFile.file
    val fileSize = Files.size(file)
    if (fileSize != failedAt.position) {
      if (fileSize < failedAt.position)
        sys.error(s"Journal file '${journalFile.file.getFileName} is shorter than the failed-over position ${failedAt.position}")
      logger.info(s"Truncating journal file at failover position ${failedAt.position} " +
        s"(${fileSize - failedAt.position} bytes): ${journalFile.file.getFileName}")
      truncated = true
      truncateFile(file, failedAt.position)
    }
    truncated ? file
  }

  private[cluster] def truncateFile(file: Path, position: Long): Unit =
    autoClosing(FileChannel.open(file, READ, WRITE)) { f =>
      // Safe the truncated part for debugging
      val out = FileChannel.open(Paths.get(s"$file~TRUNCATED-AFTER-FAILOVER"),
        WRITE, CREATE, TRUNCATE_EXISTING)
      autoClosing(out) { _ =>
        val buffer = ByteBuffer.allocate(4096)
        f.position(position - 1)
        f.read(buffer)
        buffer.flip()
        if (!buffer.hasRemaining || buffer.get() != '\n')
          sys.error(s"Invalid failed-over position=$position in '${ file.getFileName } journal file")

        var eof = false
        while (!eof) {
          if (buffer.hasRemaining) out.write(buffer)
          buffer.clear()
          eof = f.read(buffer) <= 0
          buffer.flip()
        }

        f.truncate(position)
      }
    }
}

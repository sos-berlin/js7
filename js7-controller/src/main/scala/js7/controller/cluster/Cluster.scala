package js7.controller.cluster

import akka.actor.ActorSystem
import akka.util.Timeout
import com.softwaremill.diffx
import com.typesafe.config.Config
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Paths}
import js7.base.eventbus.EventPublisher
import js7.base.generic.Completed
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.common.akkahttp.https.HttpsConfig
import js7.common.event.{EventIdGenerator, RealEventWatch}
import js7.common.scalautil.Logger
import js7.controller.cluster.ActivationInhibitor.inhibitActivationOfPeer
import js7.controller.cluster.Cluster._
import js7.controller.cluster.ClusterCommon.truncateFile
import js7.core.event.journal.JournalConf
import js7.core.event.journal.data.JournalMeta
import js7.core.event.journal.files.JournalFiles
import js7.core.event.journal.files.JournalFiles.JournalMetaOps
import js7.core.event.journal.recover.{JournaledStateRecoverer, Recovered}
import js7.core.event.state.JournaledStatePersistence
import js7.core.problems.{BackupClusterNodeNotAppointed, ClusterNodeIsNotBackupProblem, PrimaryClusterNodeMayNotBecomeBackupProblem}
import js7.data.cluster.ClusterCommand.{ClusterInhibitActivation, ClusterStartBackupNode}
import js7.data.cluster.ClusterState.{Coupled, Empty, FailedOver, HasNodes}
import js7.data.cluster.{ClusterCommand, ClusterSetting, ClusterState}
import js7.data.controller.ControllerId
import js7.data.event.{EventId, JournaledState}
import js7.data.node.NodeId
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.reflect.runtime.universe._

final class Cluster[S <: JournaledState[S]: diffx.Diff: TypeTag](
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
  private val common = new ClusterCommon(controllerId, ownId, persistence, clusterConf, httpsConfig, config, testEventPublisher)
  import common.activationInhibitor
  @volatile
  private var _activeClusterNode: Checked[ActiveClusterNode[S]] = Left(Problem.pure("This cluster node is not active"))
  private val expectingStartBackupCommand = SetOnce[Promise[ClusterStartBackupNode]]

  def stop: Task[Completed] =
    Task.defer {
      logger.debug("stop")
      for (o <- _activeClusterNode) o.close()
      common.stop
    }

  /**
    * Returns a pair of `Task`s
    * - `(Task[None], _)` no replicated state available, because it's an active node or the backup has not yet appointed.
    * - `(Task[Some[Checked[S]]], _)` with the current replicated state if this node has started as a passive node.
    * - `(_, Task[Checked[ClusterFollowUp]]` when this node should be activated
    * @return A pair of `Task`s with maybe the current `S` of this passive node (if so)
    *         and ClusterFollowUp.
    */
  def start(recovered: Recovered[S]): (Task[Option[Checked[S]]], Task[Checked[ClusterFollowUp[S]]]) = {
    if (recovered.clusterState != Empty) logger.info(s"Recovered ClusterState is ${recovered.clusterState}")

    val (currentPassiveReplicatedState, followUp) = startCluster(recovered)

    currentPassiveReplicatedState ->
      followUp.flatMapT {
        case (clusterState, followUp: ClusterFollowUp.BecomeActive[S]) =>
          val activeClusterNode = new ActiveClusterNode[S](ownId, clusterState, httpsConfig,
            persistence, eventWatch, common, clusterConf)
          activeClusterNode.start(followUp.recovered)
            .map { (_: Completed) =>
              _activeClusterNode = Right(activeClusterNode)
              Right(followUp)
            }
        }
  }

  private def startCluster(recovered: Recovered[S])
  : (Task[Option[Checked[S]]], Task[Checked[(ClusterState, ClusterFollowUp[S])]]) =
    recovered.clusterState match {
      case Empty =>
        if (clusterConf.isPrimary) {
          logger.debug(s"Active primary cluster node '$ownId', still with no backup node appointed")
          Task.pure(None) ->
            (activationInhibitor.startActive >>
              Task.pure(Right(recovered.clusterState -> ClusterFollowUp.BecomeActive(recovered))))
        } else if (recovered.eventId != EventId.BeforeFirst)
          Task.pure(Some(Left(PrimaryClusterNodeMayNotBecomeBackupProblem))) -> Task.pure(Left(PrimaryClusterNodeMayNotBecomeBackupProblem))
        else
          startBackupNodeWithEmptyClusterState(recovered)

      case clusterState: HasNodes if clusterState.activeId == ownId =>
        startActiveNodeWithBackup(recovered)

      case clusterState: HasNodes if clusterState.passiveId == ownId =>
        logger.info(
          if (clusterState.isInstanceOf[Coupled])
            s"Remaining a passive cluster node following the active node '${clusterState.activeId}'"
          else
            s"Remaining a passive cluster node trying to follow the active node '${clusterState.activeId}'")
        val passive = newPassiveClusterNode(clusterState.setting, recovered)
        passive.state.map(s => Some(Right(s))) ->
          passive.run(recovered.state)

      case _ =>
        Task.pure(None) ->
          Task.pure(Left(Problem.pure(s"Unexpected clusterState=${recovered.clusterState} (id=$ownId)")))
    }

  private def startBackupNodeWithEmptyClusterState(recovered: Recovered[S])
  : (Task[Option[Checked[S]]], Task[Checked[(ClusterState, ClusterFollowUp[S])]]) = {
    logger.info(s"Backup cluster node '$ownId', awaiting appointment from a primary node")
    val startedPromise = Promise[ClusterStartBackupNode]()
    expectingStartBackupCommand := startedPromise
    val passiveClusterNode = Task.deferFuture(startedPromise.future)
      .map(cmd => newPassiveClusterNode(cmd.setting, recovered,
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
          passive.run(recovered.state))
  }

  private def startActiveNodeWithBackup(recovered: Recovered[S])
  : (Task[Option[Checked[S]]], Task[Checked[(ClusterState, ClusterFollowUp[S])]]) = {
    recovered.clusterState match {
      case recoveredClusterState: Coupled =>
        import recoveredClusterState.{passiveId, passiveUri, timing}
        logger.info(s"This cluster node '$ownId' was coupled before restart - asking '$passiveId' about its state")
        val failedOver = inhibitActivationOfPeer(passiveUri, timing, httpsConfig, clusterConf).map {
          case None =>
            // The other node has not failed-over
            logger.info(s"The other cluster node '$passiveId' is still passive, so this node remains the active cluster node")
            //remainingActiveAfterRestart = true
            //proceedCoupled(recoveredClusterState, recovered.eventId)
            None

          case Some(otherFailedOver) =>
            import otherFailedOver.failedAt
            logger.warn(s"The other cluster node '${otherFailedOver.activeId}' failed-over and became active while this node was absent")
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
                truncated = true
                val deleteFile = journalFiles.last.file
                logger.info(s"Removing journal file written after failover: ${deleteFile.getFileName}")
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
              newPassiveClusterNode(otherFailedOver.setting, trunkRecovered, otherFailedOver = true))

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
              assertThat(otherClusterState == recovered.clusterState)
              passiveClusterNode.run(recovered.state)
          }

      case _ =>
        logger.info("Remaining the active cluster node without following node")
        //proceed(recoveredClusterState, recovered.eventId)
        Task.pure(None) ->
          (activationInhibitor.startActive >>
            Task.pure(Right(recovered.clusterState -> ClusterFollowUp.BecomeActive(recovered))))
    }
  }

  private def newPassiveClusterNode(
    setting: ClusterSetting,
    recovered: Recovered[S],
    otherFailedOver: Boolean = false,
    initialFileEventId: Option[EventId] = None)
  : PassiveClusterNode[S] =
    new PassiveClusterNode(ownId, setting, journalMeta, initialFileEventId, recovered,
      otherFailedOver,
      common.clusterWatchSynchronizer(setting),
      journalConf, clusterConf, eventIdGenerator, common)

  def activeClusterNode: Checked[ActiveClusterNode[S]] =
    _activeClusterNode

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
              if (command.setting.passiveId != ownId)
                Left(Problem.pure(s"$command sent to wrong node '$ownId'"))
              else if (command.setting.activeId == ownId)
                Left(Problem.pure(s"$command must not be sent to the active node"))
              else {
                promise.trySuccess(command)
                Right(ClusterCommand.Response.Accepted)
              }
          }
        }

      case _: ClusterCommand.ClusterPrepareCoupling |
           _: ClusterCommand.ClusterCouple |
           _: ClusterCommand.ClusterRecouple =>
        Task.pure(activeClusterNode)
          .flatMapT(_.executeCommand(command))

      case ClusterCommand.ClusterInhibitActivation(duration) =>
        activationInhibitor.inhibitActivation(duration)
          .flatMapT {
            case /*inhibited=*/true => Task.pure(Right(ClusterInhibitActivation.Response(None)))
            case /*inhibited=*/false =>
              // Could not inhibit, so this node is already active
              persistence.currentState/*TODO Possible Deadlock?*/.map(_.clusterState).map {
                case failedOver: FailedOver =>
                  logger.debug(s"inhibitActivation(${duration.pretty}) => $failedOver")
                  Right(ClusterInhibitActivation.Response(Some(failedOver)))
                case clusterState =>
                  Left(Problem.pure(
                    s"ClusterInhibitActivation command failed because node is already active but not failed-over: $clusterState"))
              }
          }
    }

  def isActive: Task[Boolean] =
    Task.pure(_activeClusterNode.isRight)
}

object Cluster
{
  private val logger = Logger(getClass)
}

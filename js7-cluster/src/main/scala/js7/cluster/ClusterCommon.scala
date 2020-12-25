package js7.cluster

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigUtil}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.{CREATE, READ, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Path, Paths}
import js7.base.auth.UserAndPassword
import js7.base.eventbus.EventPublisher
import js7.base.generic.{Completed, SecretString}
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.cluster.ClusterCommon._
import js7.common.akkahttp.https.HttpsConfig
import js7.common.configutils.Configs._
import js7.common.scalautil.Logger
import js7.core.cluster.ClusterWatch.ClusterWatchInactiveNodeProblem
import js7.core.cluster.{ClusterWatchEvents, HttpClusterWatch}
import js7.data.cluster.ClusterState.{FailedOver, HasNodes}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterSetting, ClusterState}
import js7.data.controller.ControllerId
import js7.data.node.NodeId
import monix.eval.Task
import monix.execution.Scheduler

private[cluster] final class ClusterCommon(
  controllerId: ControllerId,
  ownId: NodeId,
  currentClusterState: Task[ClusterState],
  val clusterContext: ClusterContext,
  httpsConfig: HttpsConfig,
  config: Config,
  testEventPublisher: EventPublisher[Any])
  (implicit
    scheduler: Scheduler,
    actorSystem: ActorSystem)
{
  val activationInhibitor = new ActivationInhibitor

  @volatile
  private var _clusterWatchSynchronizer: Option[ClusterWatchSynchronizer] = None

  def stop: Task[Completed] =
    _clusterWatchSynchronizer match {
      case None => Task.pure(Completed)
      case Some(o) =>
        Task.defer {
          o.stop()
          o.clusterWatch.logout().onErrorHandle(_ => Completed)
        }
    }

  def clusterWatchSynchronizer(setting: ClusterSetting): ClusterWatchSynchronizer =
    _clusterWatchSynchronizer match {
      case None =>
        setNewClusterWatchSynchronizer(setting)
      case Some(synchronizer) =>
        if (synchronizer.uri != setting.clusterWatchUri) {
          logger.debug(s"Starting new ClusterWatchSynchronizer for updated URI ${setting.clusterWatchUri}")
          synchronizer.stop()
          setNewClusterWatchSynchronizer(setting)
        } else
          synchronizer
    }

  private def setNewClusterWatchSynchronizer(setting: ClusterSetting) = {
    val result = new ClusterWatchSynchronizer(ownId, currentClusterState,
      newClusterWatchApi(setting.clusterWatchUri), setting.timing)
    assertThat(result.uri == setting.clusterWatchUri)
    _clusterWatchSynchronizer = Some(result)
    result
  }

  private def newClusterWatchApi(uri: Uri): HttpClusterWatch =
    new HttpClusterWatch(
      uri,
      userAndPassword = config.optionAs[SecretString]("js7.auth.agents."/*TODO*/ + ConfigUtil.joinPath(uri.string))
        .map(password => UserAndPassword(controllerId.toUserId, password)),
      httpsConfig = httpsConfig,
      actorSystem)

  def tryEndlesslyToSendCommand(uri: Uri, command: ClusterCommand): Task[Unit] = {
    val name = command.getClass.simpleScalaName
    clusterContext.clusterNodeApi(uri, name = name)
      .evalTap(_.loginUntilReachable())
      .use(_
        .executeClusterCommand(command))
        .map((_: ClusterCommand.Response) => ())
      .onErrorRestartLoop(()) { (throwable, _, retry) =>
        logger.warn(s"'$name' command failed with ${throwable.toStringWithCauses}")
        logger.debug(throwable.toString, throwable)
        // TODO ClusterFailed event?
        retry(()).delayExecution(1.s/*TODO*/)        // TODO Handle heartbeat timeout!
      }
  }

  def inhibitActivationOfPeer(clusterState: HasNodes): Task[Option[FailedOver]] =
    ActivationInhibitor.inhibitActivationOfPassiveNode(clusterState.setting, clusterContext)

  def ifClusterWatchAllowsActivation[A](clusterState: ClusterState.HasNodes, event: ClusterEvent, checkOnly: Boolean,
    body: Task[Checked[Boolean]])
  : Task[Checked[Boolean]] =
    activationInhibitor.tryToActivate(
      ifInhibited = Task.pure(Right(false)),  // Ignore heartbeat loss
      activate = Task.pure(clusterState.applyEvent(event))
        .flatMapT(updatedClusterState =>
          clusterWatchSynchronizer(clusterState.setting).clusterWatch.applyEvents(
            ClusterWatchEvents(ownId, event :: Nil, updatedClusterState, checkOnly = checkOnly)
          ).flatMap {
            case Left(problem) =>
              if (problem is ClusterWatchInactiveNodeProblem) {
                logger.warn(s"ClusterWatch did not agree to '${event.getClass.simpleScalaName}' event: $problem")
                testEventPublisher.publish(ClusterWatchDisagreedToActivation)
                Task.pure(Right(false))  // Ignore heartbeat loss
              } else
                Task.pure(Left(problem))

            case Right(Completed) =>
              logger.info(s"ClusterWatch agreed to '${event.getClass.simpleScalaName}' event")
              testEventPublisher.publish(ClusterWatchAgreedToActivation)
              body
          }))
}

private[js7] object ClusterCommon
{
  private val logger = Logger(getClass)

  private[cluster] def truncateFile(file: Path, position: Long): Unit =
    autoClosing(FileChannel.open(file, READ, WRITE)) { f =>
      autoClosing(FileChannel.open(Paths.get(s"$file~TRUNCATED-AFTER-FAILOVER"), WRITE, CREATE, TRUNCATE_EXISTING)) { out =>
        val buffer = ByteBuffer.allocate(4096)
        f.position(position - 1)
        f.read(buffer)
        buffer.flip()
        if (!buffer.hasRemaining || buffer.get() != '\n')
          sys.error(s"Invalid failed-over position=$position in '${file.getFileName} journal file")

        // Safe the truncated part for debugging
        var eof = false
        while(!eof) {
          if (buffer.hasRemaining) out.write(buffer)
          buffer.clear()
          eof = f.read(buffer) <= 0
          buffer.flip()
        }

        f.truncate(position)
      }
    }

  def clusterEventAndStateToString(event: ClusterEvent, state: ClusterState): String =
    s"ClusterEvent: $event --> $state"

  case object ClusterWatchAgreedToActivation
  case object ClusterWatchDisagreedToActivation
}

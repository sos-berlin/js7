package js7.controller.cluster

import akka.actor.ActorSystem
import cats.effect.Resource
import com.typesafe.config.{Config, ConfigUtil}
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, GatheringByteChannel, ScatteringByteChannel}
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
import js7.base.utils.SetOnce
import js7.base.web.Uri
import js7.common.akkahttp.https.HttpsConfig
import js7.common.configutils.Configs._
import js7.common.scalautil.Logger
import js7.controller.client.{AkkaHttpControllerApi, HttpControllerApi}
import js7.controller.cluster.ClusterCommon._
import js7.controller.data.ControllerCommand
import js7.controller.data.ControllerCommand.InternalClusterCommand
import js7.core.cluster.ClusterWatch.ClusterWatchInactiveNodeProblem
import js7.core.cluster.{ClusterWatchEvents, HttpClusterWatch}
import js7.core.event.state.JournaledStatePersistence
import js7.data.cluster.ClusterState.{FailedOver, HasNodes}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterSetting, ClusterState}
import js7.data.controller.ControllerId
import js7.data.event.JournaledState
import js7.data.node.NodeId
import monix.eval.Task
import monix.execution.Scheduler
import scala.reflect.runtime.universe._

private[cluster] final class ClusterCommon[S <: JournaledState[S]: TypeTag](
  controllerId: ControllerId,
  ownId: NodeId,
  persistence: JournaledStatePersistence[S],
  clusterConf: ClusterConf,
  httpsConfig: HttpsConfig,
  config: Config,
  testEventPublisher: EventPublisher[Any])
  (implicit
    S: JournaledState.Companion[S],
    scheduler: Scheduler,
    actorSystem: ActorSystem)
{
  val activationInhibitor = new ActivationInhibitor

  private val _clusterWatchSynchronizer = SetOnce[ClusterWatchSynchronizer]

  def stop: Task[Completed] =
    _clusterWatchSynchronizer.toOption match {
      case None => Task.pure(Completed)
      case Some(o) =>
        Task.defer {
          o.stop()
          o.clusterWatch.logout().onErrorHandle(_ => Completed)
        }
    }

  def clusterWatchSynchronizer(setting: ClusterSetting): ClusterWatchSynchronizer = {
    val result = _clusterWatchSynchronizer.getOrUpdate(
      new ClusterWatchSynchronizer(ownId, persistence.clusterState, newClusterWatchApi(setting.clusterWatchUri), setting.timing))
    assertThat(result.uri == setting.clusterWatchUri)
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
    controllerApi(uri, name = name)
      .use(_
        .executeCommand(InternalClusterCommand(command)))
        .map((_: ControllerCommand.Response) => ())
      .onErrorRestartLoop(()) { (throwable, _, retry) =>
        logger.warn(s"'$name' command failed with ${throwable.toStringWithCauses}")
        logger.debug(throwable.toString, throwable)
        // TODO ClusterFailed event?
        retry(()).delayExecution(1.s/*TODO*/)        // TODO Handle heartbeat timeout!
      }
  }

  def controllerApi(uri: Uri, name: String): Resource[Task, HttpControllerApi] =
    AkkaHttpControllerApi.resource(uri, clusterConf.userAndPassword, httpsConfig, name = name)
      .map(identity[HttpControllerApi])
      .evalTap(_.loginUntilReachable())

  def inhibitActivationOfPeer(clusterState: HasNodes): Task[Option[FailedOver]] =
    ActivationInhibitor.inhibitActivationOfPeer(clusterState.passiveUri, clusterState.timing, httpsConfig, clusterConf)

  def ifClusterWatchAllowsActivation[A](clusterState: ClusterState, event: ClusterEvent,
    clusterWatchSynchronizer: ClusterWatchSynchronizer, checkOnly: Boolean,
    body: Task[Checked[Boolean]])
  : Task[Checked[Boolean]] =
    activationInhibitor.tryToActivate(
      ifInhibited = Task.pure(Right(false)),  // Ignore heartbeat loss
      activate = Task.pure(clusterState.applyEvent(event))
        .flatMapT(updatedClusterState =>
          clusterWatchSynchronizer.clusterWatch.applyEvents(
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
        copy(f, out, buffer)
        f.truncate(position)
      }
    }

  private def copy(in: ScatteringByteChannel, out: GatheringByteChannel, buffer: ByteBuffer): Unit = {
    var eof = false
    while(!eof) {
      if (buffer.hasRemaining) out.write(buffer)
      buffer.clear()
      eof = in.read(buffer) <= 0
      buffer.flip()
    }
  }

  def clusterEventAndStateToString(event: ClusterEvent, state: ClusterState): String =
    s"ClusterEvent: $event --> $state"

  case object ClusterWatchAgreedToActivation
  case object ClusterWatchDisagreedToActivation
}

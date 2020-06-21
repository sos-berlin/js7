package js7.controller.cluster

import akka.actor.ActorSystem
import cats.effect.Resource
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, GatheringByteChannel, ScatteringByteChannel}
import java.nio.file.StandardOpenOption.{CREATE, READ, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Path, Paths}
import js7.base.eventbus.EventPublisher
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.common.akkahttp.https.HttpsConfig
import js7.common.scalautil.Logger
import js7.controller.client.{AkkaHttpControllerApi, HttpControllerApi}
import js7.controller.cluster.ClusterCommon._
import js7.controller.cluster.PassiveClusterNode.{ClusterWatchAgreesToActivation, ClusterWatchDisagreeToActivation}
import js7.controller.data.ControllerCommand
import js7.controller.data.ControllerCommand.InternalClusterCommand
import js7.core.cluster.ClusterWatch.ClusterWatchHeartbeatFromInactiveNodeProblem
import js7.core.cluster.ClusterWatchApi
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterState}
import monix.eval.Task

private[cluster] final class ClusterCommon(
  activationInhibitor: ActivationInhibitor,
  val clusterWatch: ClusterWatchApi,
  clusterConf: ClusterConf,
  httpsConfig: HttpsConfig,
  testEventPublisher: EventPublisher[Any])
  (implicit actorSystem: ActorSystem)
{
  private[cluster] def ifClusterWatchAllowsActivation[A](clusterState: ClusterState, event: ClusterEvent, body: Task[Checked[Boolean]])
  : Task[Checked[Boolean]] =
    activationInhibitor.tryToActivate(
      ifInhibited = Task.pure(Right(false)),  // Ignore heartbeat loss
      activate =
        clusterState.applyEvent(event) match {
          case Left(problem) => Task.pure(Left(problem))
          case Right(updatedClusterState) =>
            val eventName = s"'${event.getClass.simpleScalaName}' event"
            clusterWatch.applyEvents(from = clusterConf.ownId, event :: Nil, updatedClusterState).flatMap {
              case Left(problem) =>
                if (problem is ClusterWatchHeartbeatFromInactiveNodeProblem) {
                  logger.info(s"ClusterWatch did not agree to $eventName: $problem")
                  testEventPublisher.publish(ClusterWatchDisagreeToActivation)
                  Task.pure(Right(false))  // Ignore heartbeat loss
                } else
                  Task.pure(Left(problem))

              case Right(Completed) =>
                logger.info(s"ClusterWatch agreed to $eventName")
                testEventPublisher.publish(ClusterWatchAgreesToActivation)
                body
            }
        })

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
}

private[cluster] object ClusterCommon
{
  private val logger = Logger(getClass)

  private[cluster] def truncateFile(file: Path, position: Long): Unit = {
    autoClosing(FileChannel.open(file, READ, WRITE)) { f =>
      autoClosing(FileChannel.open(Paths.get(file.toString + "~TRUNCATED-AFTER-FAILOVER"), WRITE, CREATE, TRUNCATE_EXISTING)) { out =>
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
}

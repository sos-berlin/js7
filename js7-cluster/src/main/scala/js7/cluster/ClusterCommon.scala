package js7.cluster

import akka.actor.ActorSystem
import cats.effect.Resource
import com.typesafe.config.{Config, ConfigUtil}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.{CREATE, READ, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Path, Paths}
import java.util.ConcurrentModificationException
import js7.base.auth.UserAndPassword
import js7.base.configutils.Configs.*
import js7.base.eventbus.EventPublisher
import js7.base.generic.{Completed, SecretString}
import js7.base.io.https.HttpsConfig
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.cluster.ClusterCommon.*
import js7.core.cluster.ClusterWatch.ClusterWatchInactiveNodeProblem
import js7.core.cluster.HttpClusterWatch
import js7.core.license.LicenseChecker
import js7.data.cluster.ClusterState.{FailedOver, HasNodes, SwitchedOver}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeApi, ClusterState}
import js7.data.controller.ControllerId
import js7.data.node.NodeId
import monix.eval.Task
import monix.execution.atomic.AtomicAny

private[cluster] final class ClusterCommon(
  controllerId: ControllerId,
  ownId: NodeId,
  val clusterContext: ClusterContext,
  httpsConfig: HttpsConfig,
  config: Config,
  val licenseChecker: LicenseChecker,
  testEventPublisher: EventPublisher[Any])
  (implicit actorSystem: ActorSystem)
{
  val activationInhibitor = new ActivationInhibitor

  private val _clusterWatchSynchronizer = AtomicAny[Option[ClusterWatchSynchronizer]](None)

  def stop: Task[Completed] =
    Task.defer {
      _clusterWatchSynchronizer.get().fold(Task.completed)(o =>
        o.stopHeartbeating *>
          o.clusterWatch.logout().onErrorHandle(_ => Completed))
    }

  def clusterWatchSynchronizer(clusterState: ClusterState.HasNodes): Task[ClusterWatchSynchronizer] = {
    import clusterState.setting
    _clusterWatchSynchronizer.get() match {
      case None =>
        val result = initialClusterWatchSynchronizer(clusterState)
        Task.pure(result)
      case some @ Some(synchronizer) =>
        if (synchronizer.uri != setting.clusterWatchUri) {
          logger.debug(s"Starting new ClusterWatchSynchronizer for updated URI ${setting.clusterWatchUri}")
          if (!_clusterWatchSynchronizer.compareAndSet(some, None))
            throw new ConcurrentModificationException("clusterWatchSynchronizer")
          synchronizer.stopHeartbeating.map(_ =>
            initialClusterWatchSynchronizer(clusterState))
        } else
          Task.pure(synchronizer)
    }
  }

  def initialClusterWatchSynchronizer(clusterState: ClusterState.HasNodes): ClusterWatchSynchronizer =
    _clusterWatchSynchronizer.get() match {
      case Some(o) =>
        // Only after ClusterFailedOver or ClusterSwitchedOver,
        // because PassiveClusterNode has already started the ClusterWatchSynchronizer
        assertThat(clusterState.isInstanceOf[FailedOver] || clusterState.isInstanceOf[SwitchedOver])
        o

      case None =>
        import clusterState.setting
        val result = new ClusterWatchSynchronizer(
          ownId,
          newClusterWatchApi(setting.clusterWatchUri),
          setting.timing)
        assertThat(result.uri == setting.clusterWatchUri)

        if (_clusterWatchSynchronizer.getAndSet(Some(result)).isDefined)
          throw new IllegalStateException("initialClusterWatchSynchronizer")

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
    tryEndlesslyToSendCommand(clusterContext.clusterNodeApi(uri, name = name), command)
  }

  def tryEndlesslyToSendCommand(
    apiResource: Resource[Task, ClusterNodeApi],
    command: ClusterCommand)
  : Task[Unit] = {
    val name = command.getClass.simpleScalaName
    apiResource
      .use(api =>
        api.loginUntilReachable(onlyIfNotLoggedIn = true) >>
          api.executeClusterCommand(command)
            .map((_: ClusterCommand.Response) => ())
            .onErrorRestartLoop(()) { (throwable, _, retry) =>
              logger.warn(s"'$name' command failed with ${throwable.toStringWithCauses}")
              logger.debug(throwable.toString, throwable)
              // TODO ClusterFailed event?
              api.tryLogout >>
                Task.sleep(1.s/*TODO*/) >>       // TODO Handle heartbeat timeout!
                retry(())
            })
  }

  def inhibitActivationOfPeer(clusterState: HasNodes): Task[Option[FailedOver]] =
    ActivationInhibitor.inhibitActivationOfPassiveNode(clusterState.setting, clusterContext)

  def ifClusterWatchAllowsActivation(
    clusterState: ClusterState.HasNodes,
    event: ClusterEvent,
    checkOnly: Boolean,
    body: Task[Checked[Boolean]])
  : Task[Checked[Boolean]] =
    activationInhibitor.tryToActivate(
      ifInhibited = Task.pure(Right(false)),
      activate = Task.pure(clusterState.applyEvent(event))
        .flatMapT(updatedClusterState =>
          clusterWatchSynchronizer(clusterState)
            .flatMap(_.applyEvents(event :: Nil, updatedClusterState, checkOnly = checkOnly))
            .flatMap {
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
      // Safe the truncated part for debugging
      val out = FileChannel.open(Paths.get(s"$file~TRUNCATED-AFTER-FAILOVER"),
        WRITE, CREATE, TRUNCATE_EXISTING)
      autoClosing(out) { _ =>
        val buffer = ByteBuffer.allocate(4096)
        f.position(position - 1)
        f.read(buffer)
        buffer.flip()
        if (!buffer.hasRemaining || buffer.get() != '\n')
          sys.error(s"Invalid failed-over position=$position in '${file.getFileName} journal file")

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

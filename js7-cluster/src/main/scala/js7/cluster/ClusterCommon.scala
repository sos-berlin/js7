package js7.cluster

import akka.actor.ActorSystem
import cats.effect.Resource
import com.typesafe.config.{Config, ConfigUtil}
import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.{CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Path, Paths}
import js7.base.auth.UserAndPassword
import js7.base.configutils.Configs.*
import js7.base.eventbus.EventPublisher
import js7.base.generic.{Completed, SecretString}
import js7.base.io.https.HttpsConfig
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SetOnce
import js7.base.web.Uri
import js7.cluster.ClusterCommon.*
import js7.common.system.startup.Halt.haltJava
import js7.core.cluster.ClusterWatch.ClusterWatchInactiveNodeProblem
import js7.core.cluster.HttpClusterWatch
import js7.core.license.LicenseChecker
import js7.data.cluster.ClusterEvent.ClusterPassiveLost
import js7.data.cluster.ClusterState.{FailedOver, HasNodes, SwitchedOver}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeApi, ClusterState}
import js7.data.controller.ControllerId
import js7.data.node.NodeId
import monix.eval.Task
import scala.annotation.tailrec

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

  private val _clusterWatchSynchronizer = SetOnce[ClusterWatchSynchronizer]

  def stop: Task[Completed] =
    _clusterWatchSynchronizer.toOption.fold(Task.completed)(_.stop)

  def clusterWatchSynchronizer(clusterState: ClusterState.HasNodes): Task[ClusterWatchSynchronizer] =
    Task {
      _clusterWatchSynchronizer
        .toOption
        .getOrElse(initialClusterWatchSynchronizer(clusterState))
    }

  def initialClusterWatchSynchronizer(clusterState: ClusterState.HasNodes): ClusterWatchSynchronizer =
    _clusterWatchSynchronizer.toOption match {
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

        _clusterWatchSynchronizer := result
        result
    }

  def updateClusterWatchSynchronizer(clusterState: HasNodes): Task[Unit] =
    clusterWatchSynchronizer(clusterState)
      .flatMap(_
        .change(
          clusterState,
          newClusterWatchApi(clusterState.setting.clusterWatchUri)))

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
      .use(api => api
          .loginUntilReachable(onlyIfNotLoggedIn = true)
          .*>(api.executeClusterCommand(command))
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

  def ifClusterWatchAllowsActivation(clusterState: ClusterState.HasNodes, event: ClusterEvent)
    (body: Task[Checked[Boolean]])
  : Task[Checked[Boolean]] =
    logger.traceTaskWithResult(
      activationInhibitor.tryToActivate(
        ifInhibited = Task.right(false),
        activate = Task.pure(clusterState.applyEvent(event))
          .flatMapT(updatedClusterState =>
            clusterWatchSynchronizer(clusterState)
              .flatMap(_.applyEvents(event :: Nil, updatedClusterState))
              .flatMap {
                case Left(problem) =>
                  if (problem is ClusterWatchInactiveNodeProblem) {
                    logger.warn(
                      s"ClusterWatch did not agree to '${event.getClass.simpleScalaName}' event: $problem")
                    testEventPublisher.publish(ClusterWatchDisagreedToActivation)
                    if (event.isInstanceOf[ClusterPassiveLost]) {
                      haltJava(
                        "While this node has lost the passive node" +
                          " and is waiting for ClusterWatch's agreement, " +
                          "the passive node failed over",
                        restart = true,
                        warnOnly = true)
                    }
                    Task.right(false)  // Ignore heartbeat loss
                  } else
                    Task.left(problem)

                case Right(Completed) =>
                  logger.info(s"ClusterWatch agreed to '${event.getClass.simpleScalaName}' event")
                  testEventPublisher.publish(ClusterWatchAgreedToActivation)
                  body
              })))
}

private[js7] object ClusterCommon
{
  private val logger = Logger(getClass)

  private def toTruncatedFilePath(file: Path): Path =
    Paths.get(s"$file~TRUNCATED-AFTER-FAILOVER")

  private[cluster] def truncateFile(file: Path, position: Long, keep: Boolean): Unit =
    autoClosing(new RandomAccessFile(file.toFile, "rw")) { f =>
      val channel = f.getChannel
      assertCutLineEnd(file, position, channel)
      if (keep) {
        copyTruncatedRest(file, position, channel)
      } else {
        logTruncatedRest(file, position, f)
      }
      channel.truncate(position)
    }

  private def assertCutLineEnd(file: Path, position: Long, in: FileChannel): Unit = {
    val buffer = ByteBuffer.allocate(1)
    in.position(position - 1)
    in.read(buffer)
    buffer.flip()
    if (!buffer.hasRemaining || buffer.get() != '\n')
      sys.error(s"Invalid failed-over position=$position in '${file.getFileName} journal file")
  }

  /** Save the truncated part for debugging. */
  private def copyTruncatedRest(file: Path, position: Long, channel: FileChannel): Unit =
    autoClosing(FileChannel.open(toTruncatedFilePath(file), WRITE, CREATE, TRUNCATE_EXISTING)) { out =>
      channel.position(position)
      val buffer = ByteBuffer.allocate(4096)
      buffer.clear()
      buffer.flip()
      var eof = false
      while (!eof) {
        if (buffer.hasRemaining) out.write(buffer)
        buffer.clear()
        eof = channel.read(buffer) <= 0
        buffer.flip()
      }
    }

  private def logTruncatedRest(file: Path, position: Long, f: RandomAccessFile): Unit = {
    f.seek(position)
    var logged = false

    @tailrec def loop(): Unit = {
      f.readLine() match {
        case null =>
        case line =>
          if (!logged) logger.info(s"Records truncated off ${file.getFileName}:")
          logged = true
          logger.info(line)
          loop()
      }
    }
    loop()
  }

  def clusterEventAndStateToString(event: ClusterEvent, state: ClusterState): String =
    s"ClusterEvent: $event --> $state"

  case object ClusterWatchAgreedToActivation
  case object ClusterWatchDisagreedToActivation
}

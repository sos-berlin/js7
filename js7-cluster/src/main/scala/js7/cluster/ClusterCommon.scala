package js7.cluster

import akka.util.Timeout
import cats.effect.Resource
import js7.base.eventbus.EventPublisher
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SetOnce
import js7.base.web.Uri
import js7.cluster.ClusterCommon.*
import js7.cluster.ClusterConf.ClusterProductName
import js7.cluster.watch.api.ClusterWatchProblems.{ClusterNodeLossNotConfirmedProblem, ClusterWatchInactiveNodeProblem}
import js7.common.system.startup.Halt.haltJava
import js7.core.license.LicenseChecker
import js7.data.cluster.ClusterEvent.{ClusterNodeLostEvent, ClusterPassiveLost}
import js7.data.cluster.ClusterState.{FailedOver, HasNodes, SwitchedOver}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeApi, ClusterState}
import monix.eval.Task

private[cluster] final class ClusterCommon private(
  val clusterWatchCounterpart: ClusterWatchCounterpart,
  val clusterNodeApi: (Uri, String) => Resource[Task, ClusterNodeApi],
  clusterConf: ClusterConf,
  licenseChecker: LicenseChecker,
  val testEventBus: EventPublisher[Any])(
  implicit val journalActorAskTimeout: Timeout)
{
  import clusterConf.ownId

  val activationInhibitor = new ActivationInhibitor
  private val _clusterWatchSynchronizer = SetOnce[ClusterWatchSynchronizer]

  def stop: Task[Unit] =
    Task.defer {
      _clusterWatchSynchronizer.toOption.fold(Task.unit)(_.stop)
    }

  def requireValidLicense: Task[Checked[Unit]] =
    Task(licenseChecker.checkLicense(ClusterProductName))

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
          clusterWatchCounterpart,
          setting.timing)
        _clusterWatchSynchronizer := result
        result
    }

  def tryEndlesslyToSendCommand(uri: Uri, command: ClusterCommand): Task[Unit] = {
    val name = command.getClass.simpleScalaName
    tryEndlesslyToSendCommand(clusterNodeApi(uri, name), command)
  }

  def tryEndlesslyToSendCommand(
    apiResource: Resource[Task, ClusterNodeApi],
    command: ClusterCommand)
  : Task[Unit] = {
    val name = command.getClass.simpleScalaName
    apiResource
      .use(api => api
        .retryIfSessionLost()(
          api.loginUntilReachable(onlyIfNotLoggedIn = true)
            .*>(api.executeClusterCommand(command)))
        .map((_: ClusterCommand.Response) => ())
        .onErrorRestartLoop(()) { (throwable, _, retry) =>
          logger.warn(s"'$name' command failed with ${throwable.toStringWithCauses}")
          logger.debug(throwable.toString, throwable)
          // TODO ClusterFailed event?
          Task.sleep(1.s/*TODO*/) *> // TODO Handle heartbeat timeout?
            retry(())
        })
  }

  def inhibitActivationOfPeer(clusterState: HasNodes): Task[Option[FailedOver]] =
    ActivationInhibitor.inhibitActivationOfPassiveNode(clusterState.setting, clusterNodeApi)

  def ifClusterWatchAllowsActivation(
    clusterState: ClusterState.HasNodes,
    event: ClusterNodeLostEvent)
    (body: Task[Checked[Boolean]])
  : Task[Checked[Boolean]] =
    logger.traceTaskWithResult(
      activationInhibitor.tryToActivate(
        ifInhibited = Task.right(false),
        activate = Task.pure(clusterState.applyEvent(event))
          .flatMapT {
            case updatedClusterState: HasNodes =>
              clusterWatchSynchronizer(clusterState)
                .flatMap(_.applyEvent(event, updatedClusterState))
                .flatMap {
                  case Left(problem) =>
                    if (problem.is(ClusterNodeLossNotConfirmedProblem)
                      || problem.is(ClusterWatchInactiveNodeProblem)) {
                      logger.warn(
                        s"🚫 ClusterWatch did not agree to '${event.getClass.simpleScalaName}' event: $problem")
                      testEventBus.publish(ClusterWatchDisagreedToActivation)
                      if (event.isInstanceOf[ClusterPassiveLost]) {
                        haltJava(
                          "🟥 While this node has lost the passive node" +
                            " and is waiting for ClusterWatch's agreement, " +
                            "the passive node failed over",
                          restart = true,
                          warnOnly = true)
                      }
                      Task.right(false)  // Ignore heartbeat loss
                    } else
                      Task.left(problem)

                  case Right(None) =>
                    logger.debug(
                      s"No ClusterWatch confirmation required for '${event.getClass.simpleScalaName}' event")
                    body

                  case Right(maybeConfirm) =>
                    maybeConfirm match {
                      case None =>
                        logger.info(
                          s"ClusterWatch agreed to '${event.getClass.simpleScalaName}' event")
                      case Some(confirm) =>
                        logger.info(
                          s"${confirm.confirmer} agreed to '${event.getClass.simpleScalaName}' event")
                    }
                    testEventBus.publish(ClusterWatchAgreedToActivation)
                    body
                }
            case ClusterState.Empty => Task.left(Problem.pure(
              "ClusterState.Empty in ifClusterWatchAllowsActivation ??"))
          }))
}

private[js7] object ClusterCommon
{
  private val logger = Logger(getClass)

  def resource(
    clusterWatchCounterpart: ClusterWatchCounterpart,
    clusterNodeApi: (Uri, String) => Resource[Task, ClusterNodeApi],
    clusterConf: ClusterConf,
    licenseChecker: LicenseChecker,
    testEventPublisher: EventPublisher[Any])(
    implicit akkaTimeout: Timeout)
  : Resource[Task, ClusterCommon] =
      Resource.make(
        Task(new ClusterCommon(
          clusterWatchCounterpart,
          clusterNodeApi, clusterConf, licenseChecker, testEventPublisher)))(
        release = _.stop)

  def clusterEventAndStateToString(event: ClusterEvent, state: ClusterState): String =
    s"ClusterEvent: $event --> $state"

  case object ClusterWatchAgreedToActivation
  case object ClusterWatchDisagreedToActivation
}

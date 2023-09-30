package js7.cluster

import akka.util.Timeout
import cats.effect.{ExitCase, Resource}
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.eventbus.EventPublisher
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{OneTimeToken, OneTimeTokenProvider, SetOnce}
import js7.cluster.ClusterCommon.*
import js7.cluster.ClusterConf.ClusterProductName
import js7.common.system.startup.Halt.haltJava
import js7.core.license.LicenseChecker
import js7.data.cluster.ClusterEvent.{ClusterNodeLostEvent, ClusterPassiveLost}
import js7.data.cluster.ClusterState.{FailedOver, HasNodes, SwitchedOver}
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeLossNotConfirmedProblem, ClusterWatchInactiveNodeProblem}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeApi, ClusterState}
import monix.eval.Task
import scala.concurrent.duration.Deadline.now
import scala.reflect.ClassTag

private[cluster] final class ClusterCommon private(
  val clusterWatchCounterpart: ClusterWatchCounterpart,
  val clusterNodeApi: (Admission, String) => Resource[Task, ClusterNodeApi],
  clusterConf: ClusterConf,
  licenseChecker: LicenseChecker,
  val testEventBus: EventPublisher[Any])(
  implicit val journalActorAskTimeout: Timeout):
  
  import clusterConf.ownId

  val activationInhibitor = new ActivationInhibitor
  private val _clusterWatchSynchronizer = SetOnce[ClusterWatchSynchronizer]
  val couplingTokenProvider = OneTimeTokenProvider.unsafe()

  def stop: Task[Unit] =
    Task.defer:
      _clusterWatchSynchronizer.toOption.fold(Task.unit)(_.stop)

  def requireValidLicense: Task[Checked[Unit]] =
    Task(licenseChecker.checkLicense(ClusterProductName))

  def clusterWatchSynchronizer(clusterState: ClusterState.HasNodes): Task[ClusterWatchSynchronizer] =
    Task:
      _clusterWatchSynchronizer
        .toOption
        .getOrElse(initialClusterWatchSynchronizer(clusterState))

  def initialClusterWatchSynchronizer(clusterState: ClusterState.HasNodes): ClusterWatchSynchronizer =
    _clusterWatchSynchronizer.toOption match
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

  def tryEndlesslyToSendCommand(admission: Admission, command: ClusterCommand): Task[Unit] =
    val name = command.getClass.simpleScalaName
    tryEndlesslyToSendCommand(clusterNodeApi(admission, name), _ => command)

  def tryEndlesslyToSendCommand[C <: ClusterCommand: ClassTag](
    apiResource: Resource[Task, ClusterNodeApi],
    toCommand: OneTimeToken => C)
  : Task[Unit] =
    Task.defer:
      val name = implicitClass[C].simpleScalaName
      var warned = false
      val since = now
      apiResource
        .use(api => api
          .retryIfSessionLost()(
            api.loginUntilReachable(onlyIfNotLoggedIn = true) *>
              couplingTokenProvider.resource.use(token =>
                api.executeClusterCommand(toCommand(token))))
          .map((_: ClusterCommand.Response) => ())
          .onErrorRestartLoop(()) { (throwable, _, retry) =>
            warned = true
            logger.warn(s"🔴 $name command failed with ${throwable.toStringWithCauses}")
            logger.debug(throwable.toString, throwable)
            // TODO ClusterFailed event?
            Task.sleep(1.s/*TODO*/) *> // TODO Handle heartbeat timeout?
              retry(())
          }
          .guaranteeCase {
            case ExitCase.Completed => Task(
              if warned then logger.info(s"🟢 $name command succeeded after ${since.elapsed.pretty}"))
            case ExitCase.Canceled => Task(
              if warned then logger.info(s"⚫️ $name Canceled after ${since.elapsed.pretty}"))
            case _ => Task.unit
          })

  def inhibitActivationOfPeer(clusterState: HasNodes, peersUserAndPassword: Option[UserAndPassword])
  : Task[Option[FailedOver]] =
    ActivationInhibitor.inhibitActivationOfPassiveNode(
      clusterState.setting, peersUserAndPassword, clusterNodeApi)

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
                    if problem.is(ClusterNodeLossNotConfirmedProblem)
                      || problem.is(ClusterWatchInactiveNodeProblem) then {
                      logger.warn(
                        s"⛔ ClusterWatch did not agree to '${event.getClass.simpleScalaName}' event: $problem")
                      testEventBus.publish(ClusterWatchDisagreedToActivation)
                      if event.isInstanceOf[ClusterPassiveLost] then {
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

private[js7] object ClusterCommon:
  private val logger = Logger[this.type]

  def resource(
    clusterWatchCounterpart: ClusterWatchCounterpart,
    clusterNodeApi: (Admission, String) => Resource[Task, ClusterNodeApi],
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

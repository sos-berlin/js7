package js7.cluster

import cats.effect.std.Supervisor
import cats.effect.{IO, ResourceIO}
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.monixutils.AsyncVariable
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.DelayConf
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.cluster.ActivationInhibitor.*
import js7.common.http.PekkoHttpClient
import js7.data.cluster.ClusterCommand.ClusterInhibitActivation
import js7.data.cluster.ClusterState.FailedOver
import js7.data.cluster.{ClusterNodeApi, ClusterSetting}
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration

/** Inhibits activation of cluster node for the specified duration. */
private[cluster] final class ActivationInhibitor private(supervisor: Supervisor[IO]):

  private val _state = AsyncVariable[State](Initial)

  def startActive: IO[Unit] =
    startAs(Active)

  def startPassive: IO[Unit] =
    startAs(Passive)

  private def startAs(state: State): IO[Unit] =
    IO.defer:
      logger.debug(s"startAs $state")
      _state.update:
        case Initial =>
          IO.pure(state)
        case s =>
          IO.raiseError:
            new IllegalStateException(s"ActivationInhibitor startAs($state): Already '$s''")
      .void

  def tryToActivate(activate: IO[Checked[Boolean]]): IO[Checked[Boolean]] =
    logger.debugIOWithResult:
      _state.updateCheckedWithResult:
        case Initial | Passive | Active =>
          activate.flatMap:
            case o @ (Left(_) | Right(false)) => IO:
              logger.debug(s"tryToActivate: Passive — due to $o")
              o.map(Passive -> _)
            case Right(true) =>
              IO:
                logger.debug("tryToActivate: Active — due to Right(true)")
                Right(Active -> true)
        case inhibited: Inhibited =>
          IO:
            logger.info("Activation inhibited")
            Right(inhibited -> false)

  /** Tries to inhibit activation for `duration`.
    * @return true if activation is or has been inhibited, false if already active
    */
  def inhibitActivation(duration: FiniteDuration): IO[Checked[Boolean]] =
    logger.debugIOWithResult:
      _state.updateCheckedWithResult:
        case state @ (Initial | Passive | _: Inhibited) =>
          setInhibitionTimer(duration) *>
            IO:
              val depth = state match
                case Inhibited(n) => n + 1
                case _ => 1
              Right(Inhibited(depth) -> true)

        case Active =>
          IO.right(Active -> false)

  private def setInhibitionTimer(duration: FiniteDuration): IO[Unit] =
    supervisor.supervise:
      _state.update:
          case Inhibited(1) => IO.pure(Passive)
          case Inhibited(n) => IO.pure(Inhibited(n - 1))
          case state =>
            // May happend in very race case of race condition
            IO:
              logger.error:
                s"inhibitActivation timeout after ${duration.pretty}: expected Inhibited but got '$state'"
              state
      .delayBy(duration)
    .void

  @TestOnly
  private[cluster] def state: IO[Option[State]] =
    _state.lockedValue.map(Some(_))
      .timeoutTo(100.ms, IO.none) // Lock detection


private[cluster] object ActivationInhibitor:
  private val logger = Logger[this.type]

  def resource: ResourceIO[ActivationInhibitor] =
    Supervisor[IO].map:
      new ActivationInhibitor(_)

  def inhibitActivationOfPassiveNode(
    setting: ClusterSetting,
    peersUserAndPassword: Option[UserAndPassword],
    clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi])
  : IO[Option[FailedOver]] =
    val admission = Admission(setting.passiveUri, peersUserAndPassword)
    logger.debugIO(s"inhibitActivationOfPassiveNode ${setting.passiveUri}"):
      DelayConf.default.runIO: delayer =>
        clusterNodeApi(admission, "inhibitActivationOfPassiveNode")
          .evalTap(_.loginUntilReachable())
          .use:
            _.executeClusterCommand:
              ClusterInhibitActivation(setting.timing.inhibitActivationDuration)
            .map(_.failedOver)
          .onErrorRestartLoop(()): (throwable, _, retry) =>
            // TODO Code mit loginUntilReachable usw. zusammenfassen.
            //  Stacktrace unterdrücken wenn isNotIgnorableStackTrace
            val msg = "While trying to reach the other cluster node due to restart: " +
              throwable.toStringWithCauses
            logger.warn(msg)
            for t <- throwable.ifStackTrace if PekkoHttpClient.hasRelevantStackTrace(t) do
              logger.debug(msg, t)
            delayer.sleep >> retry(())

  private[cluster] sealed trait State
  private[cluster] case object Initial extends State
  private[cluster] case object Passive extends State
  private[cluster] case class Inhibited(depth: Int) extends State:
    assertThat(depth >= 1)
  private[cluster] case object Active extends State

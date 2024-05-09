package js7.cluster

import cats.effect.{IO, Outcome, ResourceIO}
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.MVar
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.cluster.ActivationInhibitor.*
import js7.data.cluster.ClusterCommand.ClusterInhibitActivation
import js7.data.cluster.ClusterState.FailedOver
import js7.data.cluster.{ClusterNodeApi, ClusterSetting}
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration

/** Inhibits activation of cluster node for the specified duration. */
private[cluster] final class ActivationInhibitor:

  private val stateMvarIO = MVar[IO].of[State](Initial).unsafeMemoize

  def startActive: IO[Unit] =
    startAs(Active)

  def startPassive: IO[Unit] =
    startAs(Passive)

  private def startAs(state: State): IO[Unit] =
    IO.defer:
      logger.debug(s"startAs $state")
      stateMvarIO.flatMap(mvar =>
        mvar.tryTake.flatMap {
          case Some(Initial) =>
            mvar.put(state)
          case s =>
            s.fold(IO.unit)(mvar.put) >>
              IO.raiseError(new IllegalStateException(
                s"ActivationInhibitor startAs($state): Already '$s''"))
        })

  def tryToActivate(ifInhibited: IO[Checked[Boolean]], activate: IO[Checked[Boolean]])
  : IO[Checked[Boolean]] =
    logger.debugIO(
      IO.defer {
        stateMvarIO.flatMap(mvar =>
          mvar.take.flatMap {
            case Initial | Passive | Active =>
              activate
                .guaranteeCase {
                  case Outcome.Succeeded(_) => IO.unit
                  case outcome => IO.defer:
                    logger.debug(s"tryToActivate: Passive — due to $outcome")
                    mvar.put(Passive)
                }
                .flatTap {
                  case o @ (Left(_) | Right(false)) => IO.defer:
                    logger.debug(s"tryToActivate: Passive — due to $o")
                    mvar.put(Passive)
                  case Right(true) =>
                    logger.debug("tryToActivate: Active — due to Right(true)")
                    mvar.put(Active)
                }

            case o: Inhibited => IO.defer:
              logger.debug(s"tryToActivate: $o")
              mvar.put(o) *>
                IO { logger.info("Activation inhibited") } *>
                ifInhibited
          })
      })

  /** Tries to inhibit activation for `duration`.
    * @return true if activation is or has been inhibited, false if already active
    */
  def inhibitActivation(duration: FiniteDuration): IO[Checked[Boolean]] =
    logger.debugIOWithResult[Checked[Boolean]](
      stateMvarIO.flatMap(mvar =>
        mvar.take
          .flatMap {
            case state @ (Initial | Passive | _: Inhibited) =>
              val depth = state match
                case Inhibited(n) => n + 1
                case _ => 1
              mvar
                .put(Inhibited(depth))
                .flatMap(_ => setInhibitionTimer(duration))
                .map(_ => Right(true))

            case Active =>
              mvar.put(Active)
                .map(_ => Right(false))
          }))

  private def setInhibitionTimer(duration: FiniteDuration): IO[Unit] =
    stateMvarIO
      .flatMap: mvar =>
        mvar.take.flatMap:
          case Inhibited(1) => mvar.put(Passive)
          case Inhibited(n) => mvar.put(Inhibited(n - 1))
          case state => IO(logger.error: // Must not happen
            s"inhibitActivation timeout after ${duration.pretty}: expected Inhibited but got '$state'")
      .attempt.map:
        case Left(throwable) => logger.error(
          s"setInhibitionTimer: ${throwable.toStringWithCauses}", throwable.nullIfNoStackTrace)
        case Right(()) =>
      .delayBy(duration)
      .startAndForget

  @TestOnly
  private[cluster] def state: IO[Option[State]] =
    stateMvarIO.flatMap(_.tryRead)

private[cluster] object ActivationInhibitor:
  private val logger = Logger[this.type]

  def inhibitActivationOfPassiveNode(
    setting: ClusterSetting,
    peersUserAndPassword: Option[UserAndPassword],
    clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi])
  : IO[Option[FailedOver]] =
    IO.defer:
      val retryDelay = 5.s // TODO
      clusterNodeApi(
        Admission(setting.passiveUri, peersUserAndPassword),
        "inhibitActivationOfPassiveNode"
      ).evalTap(_.loginUntilReachable())
        .use(_
          .executeClusterCommand(
            ClusterInhibitActivation(setting.timing.inhibitActivationDuration))
          .map(_.failedOver))
        .onErrorRestartLoop(()): (throwable, _, retry) =>
          // TODO Code mit loginUntilReachable usw. zusammenfassen.
          //  Stacktrace unterdrücken wenn isNotIgnorableStackTrace
          val msg = "While trying to reach the other cluster node due to restart: " +
            throwable.toStringWithCauses
          logger.warn(msg)
          for t <- throwable.ifStackTrace do logger.debug(msg, t)
          retry(()).delayBy(retryDelay)
    .map: maybeFailedOver =>
      logger.debug(s"${setting.passiveUri} ClusterInhibitActivation returned failedOver=$maybeFailedOver")
      maybeFailedOver

  private[cluster] sealed trait State
  private[cluster] case object Initial extends State
  private[cluster] case object Passive extends State
  private[cluster] case class Inhibited(depth: Int) extends State:
    assertThat(depth >= 1)
  private[cluster] case object Active extends State

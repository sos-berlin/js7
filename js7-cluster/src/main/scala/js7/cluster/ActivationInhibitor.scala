package js7.cluster

import cats.effect.{ExitCase, Resource}
import cats.syntax.flatMap.*
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.cluster.ActivationInhibitor.*
import js7.data.cluster.ClusterCommand.ClusterInhibitActivation
import js7.data.cluster.ClusterState.FailedOver
import js7.data.cluster.{ClusterNodeApi, ClusterSetting}
import monix.catnap.MVar
import monix.eval.Task
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration

/** Inhibits activation of cluster node for the specified duration. */
private[cluster] final class ActivationInhibitor
{
  private val stateMvarTask = MVar[Task].of[State](Initial).memoize

  def startActive: Task[Unit] =
    startAs(Active)

  def startPassive: Task[Unit] =
    startAs(Passive)

  private def startAs(state: State): Task[Unit] =
    Task.defer {
      logger.debug(s"startAs $state")
      stateMvarTask.flatMap(mvar =>
        mvar.tryTake.flatMap {
          case Some(Initial) =>
            mvar.put(state)
          case s =>
            s.fold(Task.unit)(mvar.put) >>
              Task.raiseError(new IllegalStateException(
                s"ActivationInhibitor startAs($state): Already '$s''"))
        })
    }

  def tryToActivate(ifInhibited: Task[Checked[Boolean]], activate: Task[Checked[Boolean]])
  : Task[Checked[Boolean]] =
    logger.debugTask(
      Task.defer {
        stateMvarTask.flatMap(mvar =>
          mvar.take.flatMap {
            case Initial | Passive | Active =>
              activate
                .guaranteeCase {
                  case ExitCase.Completed => Task.unit
                  case exitCase =>
                    logger.debug(s"tryToActivate: Passive — due to $exitCase")
                    mvar.put(Passive)
                }
                .flatTap {
                  case o @ (Left(_) | Right(false)) =>
                    logger.debug(s"tryToActivate: Passive — due to $o")
                    mvar.put(Passive)
                  case Right(true) =>
                    logger.debug("tryToActivate: Active — due to Right(true)")
                    mvar.put(Active)
                }

            case o: Inhibited =>
              logger.debug(s"tryToActivate: $o")
              mvar.put(o) *>
                Task { logger.info("Activation inhibited") } *>
                ifInhibited
          })
      })

  /** Tries to inhibit activation for `duration`.
    * @return true if activation is or has been inhibited, false if already active
    */
  def inhibitActivation(duration: FiniteDuration): Task[Checked[Boolean]] =
    logger.debugTaskWithResult[Checked[Boolean]](
      stateMvarTask.flatMap(mvar =>
        mvar.take
          .flatMap {
            case state @ (Initial | Passive | _: Inhibited) =>
              val depth = state match {
                case Inhibited(n) => n + 1
                case _ => 1
              }
              mvar
                .put(Inhibited(depth))
                .flatMap(_ => setInhibitionTimer(duration))
                .map(_ => Right(true))

            case Active =>
              mvar.put(Active)
                .map(_ => Right(false))
          }))

  private def setInhibitionTimer(duration: FiniteDuration): Task[Unit] =
    Task.deferAction { implicit scheduler =>
      Task {
        scheduler.scheduleOnce(duration) {
          stateMvarTask.flatMap(mvar =>
            mvar.take.flatMap {
              case Inhibited(1) => mvar.put(Passive)
              case Inhibited(n) => mvar.put(Inhibited(n - 1))
              case state => Task {
                // Must not happen
                logger.error(
                  s"inhibitActivation timeout after ${duration.pretty}: expected Inhibited but got '$state'")
              }
            })
            .runAsyncUncancelable {
              case Left(throwable) => logger.error(
                s"setInhibitionTimer: ${throwable.toStringWithCauses}", throwable.nullIfNoStackTrace)
              case Right(()) =>
            }
        }
      }
    }

  @TestOnly
  private[cluster] def state: Task[Option[State]] =
    stateMvarTask.flatMap(_.tryRead)
}

private[cluster] object ActivationInhibitor
{
  private val logger = Logger[this.type]

  def inhibitActivationOfPassiveNode(
    setting: ClusterSetting,
    peersUserAndPassword: Option[UserAndPassword],
    clusterNodeApi: (Admission, String) => Resource[Task, ClusterNodeApi])
  : Task[Option[FailedOver]] =
    Task.defer {
      val retryDelay = 5.s  // TODO
      clusterNodeApi(
        Admission(setting.passiveUri, peersUserAndPassword),
        "inhibitActivationOfPassiveNode"
      ).evalTap(_.loginUntilReachable())
        .use(_
          .executeClusterCommand(
            ClusterInhibitActivation(setting.timing.inhibitActivationDuration))
          .map(_.failedOver))
        .onErrorRestartLoop(()) { (throwable, _, retry) =>
          // TODO Code mit loginUntilReachable usw. zusammenfassen.
          //  Stacktrace unterdrücken wenn isNotIgnorableStackTrace
          val msg = "While trying to reach the other cluster node due to restart: " +
            throwable.toStringWithCauses
          logger.warn(msg)
          for t <- throwable.ifStackTrace do logger.debug(msg, t)
          retry(()).delayExecution(retryDelay)
        }
    }.map { maybeFailedOver =>
      logger.debug(s"${setting.passiveUri} ClusterInhibitActivation returned failedOver=$maybeFailedOver")
      maybeFailedOver
    }

  private[cluster] sealed trait State
  private[cluster] case object Initial extends State
  private[cluster] case object Passive extends State
  private[cluster] case class Inhibited(depth: Int) extends State {
    assertThat(depth >= 1)
  }
  private[cluster] case object Active extends State
}

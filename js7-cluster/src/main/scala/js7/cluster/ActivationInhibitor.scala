package js7.cluster

import cats.effect.ExitCase
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.cluster.ActivationInhibitor._
import js7.data.cluster.ClusterCommand.ClusterInhibitActivation
import js7.data.cluster.ClusterSetting
import js7.data.cluster.ClusterState.FailedOver
import monix.catnap.MVar
import monix.eval.Task
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
    stateMvarTask.flatMap(mvar =>
      mvar.tryTake.flatMap {
        case Some(Initial) =>
          mvar.put(state)
        case s =>
          s.fold(Task.unit)(mvar.put) >>
            Task.raiseError(new IllegalStateException(s"ActivationInhibitor markAs($state): Already '$s''"))
      })

  def tryToActivate[A](ifInhibited: Task[A], activate: Task[A]): Task[A] =
    stateMvarTask.flatMap(mvar =>
      mvar.take.flatMap {
        case Initial | Passive | Active =>
          activate
            .guaranteeCase {
              case ExitCase.Completed => mvar.put(Active)
              case _ => mvar.put(Passive)
            }

        case o: Inhibited =>
          mvar.put(o) >>
            Task { logger.info("Activation inhibited") } >>
            ifInhibited
      })

  /** Tries to inhibit activation for `duration`.
    * @return true if activation is or has been inhibited, false if already active
    */
  def inhibitActivation(duration: FiniteDuration): Task[Checked[Boolean]] =
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
        })

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

  private[cluster] def state: Task[Option[State]] =
    stateMvarTask.flatMap(_.tryRead)
}

private[cluster] object ActivationInhibitor
{
  private val logger = Logger(getClass)

  def inhibitActivationOfPassiveNode(setting: ClusterSetting, ctx: ClusterContext)
  : Task[Option[FailedOver]] =
    Task.defer {
      val retryDelay = 5.s  // TODO
      ctx.clusterNodeApi(setting.passiveUri, "inhibitActivationOfPassiveNode")
        .evalTap(_.loginUntilReachable())
        .use(_
          .executeClusterCommand(ClusterInhibitActivation(2 * setting.timing.heartbeat/*???*/))
          .map(_.failedOver))
        .onErrorRestartLoop(()) { (throwable, _, retry) =>
          // TODO Code mit loginUntilReachable usw. zusammenfassen.
          //  Stacktrace unterdrücken wenn isNotIgnorableStackTrace
          val msg = "While trying to reach the other cluster node due to restart: " +
            throwable.toStringWithCauses
          logger.warn(msg)
          for (t <- throwable.ifStackTrace) logger.debug(msg, t)
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

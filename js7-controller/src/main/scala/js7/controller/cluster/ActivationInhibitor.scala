package js7.controller.cluster

import cats.effect.ExitCase
import js7.base.problem.Checked
import js7.common.scalautil.Logger
import js7.controller.cluster.ActivationInhibitor._
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.cancelables.SerialCancelable
import scala.concurrent.duration.FiniteDuration

/** Inhibits activation of cluster node for the specified duration. */
private[cluster] final class ActivationInhibitor
{
  private val stateMvarTask = MVar[Task].of[State](Initial).memoize
  private val inhibitionTimer = SerialCancelable()

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
            Task.raiseError(throw new IllegalStateException(s"ActivationInhibitor markAs($state): Already '$s''"))
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

        case Inhibited =>
          mvar.put(Inhibited) >> ifInhibited
      })

  /** Tries to inhibit activation for `duration`.
    * @return true if activation is or has been inhibited, false if already active
    */
  def inhibitActivation(duration: FiniteDuration): Task[Checked[Boolean]] =
    stateMvarTask.flatMap(mvar =>
      mvar.take
        .flatMap {
          case Initial | Passive | Inhibited =>
            mvar.put(Inhibited)
              .flatMap(_ => setInhibitionTimer(duration))
              .map(_ => Right(true))

          case Active =>
            mvar.put(Active)
              .map(_ => Right(false))
        })

  private def setInhibitionTimer(duration: FiniteDuration): Task[Unit] =
    Task.deferAction { implicit scheduler =>
      Task {
        inhibitionTimer := scheduler.scheduleOnce(duration) {
          stateMvarTask.flatMap(mvar =>
            mvar.take.flatMap {
              case Inhibited => mvar.put(Passive)
              case state => Task {
                // Must not happen
                logger.error(s"ActivationInhibitor.inhibitActivation timeout: expected Inhibit state but got '$state'")
              }
            })
          .runAsyncAndForget
        }
      }
    }

  private[cluster] def state: Task[Option[State]] =
    stateMvarTask.flatMap(_.tryRead)
}

private[cluster] object ActivationInhibitor
{
  private val logger = Logger(getClass)

  private[cluster] sealed trait State
  private[cluster] case object Initial extends State
  private[cluster] case object Passive extends State
  private[cluster] case object Inhibited extends State
  private[cluster] case object Active extends State
}

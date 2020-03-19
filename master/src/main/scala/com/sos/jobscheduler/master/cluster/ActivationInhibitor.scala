package com.sos.jobscheduler.master.cluster

import cats.effect.ExitCase
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.master.cluster.ActivationInhibitor._
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.cancelables.SerialCancelable
import scala.concurrent.duration.FiniteDuration

/** Inhibits activation of cluster node for the specified duration. */
private[cluster] final class ActivationInhibitor
{
  private val stateMvarTask = MVar[Task].of[State](Passive).memoize
  private val inhibitionTimer = SerialCancelable()

  def tryToActivate[A](ifInhibited: Task[A], activate: Task[A]): Task[A] =
    stateMvarTask.flatMap(mvar =>
      mvar.take.flatMap {
        case Passive | Active =>
          activate
            .guaranteeCase {
              case ExitCase.Completed => mvar.put(Active)
              case _ => mvar.put(Passive)
            }

        case Inhibited =>
          mvar.put(Inhibited) >> ifInhibited
      })

  def inhibitActivation(duration: FiniteDuration): Task[Checked[Boolean]] =
    stateMvarTask.flatMap(mvar =>
      mvar.take
        .flatMap {
          case Passive | Inhibited =>
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
  private[cluster] case object Passive extends State
  private[cluster] case object Inhibited extends State
  private[cluster] case object Active extends State
}

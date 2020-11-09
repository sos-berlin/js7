package js7.controller.cluster

import akka.actor.ActorSystem
import cats.effect.ExitCase
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.web.Uri
import js7.common.akkahttp.https.HttpsConfig
import js7.common.scalautil.Logger
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.cluster.ActivationInhibitor._
import js7.controller.data.ControllerCommand.InternalClusterCommand
import js7.data.cluster.ClusterCommand.ClusterInhibitActivation
import js7.data.cluster.ClusterState.FailedOver
import js7.data.cluster.ClusterTiming
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
            .onErrorRecover { case t: Throwable =>
              logger.error(s"setInhibitionTimer: $t", t)
            }
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

  def inhibitActivationOfPeer(uri: Uri, timing: ClusterTiming, httpsConfig: HttpsConfig, clusterConf: ClusterConf)
    (implicit actorSystem: ActorSystem): Task[Option[FailedOver]] =
    Task.defer {
      val retryDelay = 5.s  // TODO
      AkkaHttpControllerApi.resource(uri, clusterConf.userAndPassword, httpsConfig, name = "ClusterInhibitActivation")
        .use(otherNode =>
          otherNode.loginUntilReachable() >>
            otherNode.executeCommand(
              InternalClusterCommand(
                ClusterInhibitActivation(2 * timing.heartbeat/*???*/))
            ).map(_.response.asInstanceOf[ClusterInhibitActivation.Response].failedOver))
        .onErrorRestartLoop(()) { (throwable, _, retry) =>
          // TODO Code mit loginUntilReachable usw. zusammenfassen. Stacktrace unterdrücken wenn isNotIgnorableStackTrace
          val msg = "While trying to reach the other cluster node due to restart: " + throwable.toStringWithCauses
          logger.warn(msg)
          for (t <- throwable.ifNoStackTrace) logger.debug(msg, t)
          retry(()).delayExecution(retryDelay)
        }
    }.map { maybeFailedOver =>
      logger.debug(s"$uri ClusterInhibitActivation returned failedOver=$maybeFailedOver")
      maybeFailedOver
    }

  private[cluster] sealed trait State
  private[cluster] case object Initial extends State
  private[cluster] case object Passive extends State
  private[cluster] case object Inhibited extends State
  private[cluster] case object Active extends State
}

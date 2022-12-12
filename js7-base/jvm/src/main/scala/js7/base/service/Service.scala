package js7.base.service

import cats.effect.concurrent.Deferred
import cats.effect.{ExitCase, Resource}
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.service.Service.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.Stoppable
import monix.eval.{Fiber, Task}

trait Service extends Stoppable {
  service =>

  private val stopped = Deferred.unsafe[Task, Unit]

  final def untilStopped: Task[Unit] =
    stopped.get

  protected def start: Task[ServiceStarted]

  protected final def startService(run: Task[Unit]): Task[ServiceStarted] =
    CorrelId
      .bindNew(logger.debugTask(s"$service run")(
        run
          .guarantee(stopped.complete(()))
          .guaranteeCase {
            case ExitCase.Error(t) => Task {
              // A service should not die
              logger.error(s"$service died: ${t.toStringWithCauses}")
            }
            case _ => Task.unit
          }))
      .start
      .flatMap(fiber =>
        service match {
          case service: StoppableByRequest => service._fiber.complete(fiber)
          case _ => Task.unit
        })
      .as(ServiceStarted)
}

object Service
{
  private val logger = Logger[this.type]

  def resource[A <: Service](acquire: Task[A]): Resource[Task, A] =
    Stoppable.resource(
      acquire.flatMap(service =>
        service.start.as(service)))

  trait StoppableByRequest extends Service
  {
    service =>

    private[Service] final val _fiber = Deferred.unsafe[Task, Fiber[Unit]]
    private val stopRequested = Deferred.unsafe[Task, Unit]

    protected final def whenStopRequested: Task[Unit] =
      stopRequested.get

    private val memoizedStop =
      stopRequested.complete(())
        .*>(_fiber.get)
        .flatMap(_.join)
        .logWhenItTakesLonger(s"stopping $service")
        .memoize

    def stop =
      logger.debugTask(s"$service stop")(
        memoizedStop)
  }

  implicit final class RichServiceResource[A](private val resource: Resource[Task, A])
  extends AnyVal {
    def startService(implicit evidence: A <:< Service): Task[A] =
      resource.allocated.map(_._1)
  }

  /** Marker type to ensure call of `startFiber`. */
  final class ServiceStarted private[Service]
  private val ServiceStarted = new ServiceStarted()
}

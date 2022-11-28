package js7.base.service

import cats.effect.Resource
import cats.effect.concurrent.Deferred
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import monix.eval.{Fiber, Task}

trait StatefulService {
  service =>

  private[StatefulService] final val _fiber = Deferred.unsafe[Task, Fiber[Unit]]

  protected def run: Task[Unit]

  protected def stop: Task[Unit]
}

object StatefulService
{
  private val logger = Logger[this.type]

  def resource[A <: StatefulService](newService: Task[A]): Resource[Task, A] =
    Resource.make(
      acquire = Task.defer {
        newService
          .flatMap(service => CorrelId
            .bindNew(logger
              .debugTask(s"$service run")(
                service.run))
            .onErrorHandleWith(t => Task.defer {
              // A service should not die
              logger.error(s"$service died: ${t.toStringWithCauses}")
              Task.raiseError(t)
            })
            .start
            .tapEval(service._fiber.complete)
            .as(service))
      })(
      release = _.stop)

  trait StoppableByRequest extends StatefulService
  {
    service =>

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
}

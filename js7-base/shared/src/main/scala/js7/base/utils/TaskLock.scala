package js7.base.utils

import cats.effect.Resource
import js7.base.monixutils.MonixBase.syntax._
import js7.base.time.ScalaTime._
import monix.catnap.MVar
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration

final class TaskLock private(val resource: Resource[Task, Unit])
{
  def lock[A](task: Task[A]): Task[A] =
    resource.use(_ => task)
}

object TaskLock
{
  private val defaultWarnTimeouts = Seq(3.s, 7.s, 10.s)

  def apply(name: String, warnTimeouts: IterableOnce[FiniteDuration] = defaultWarnTimeouts) =
    new TaskLock(resource(name, warnTimeouts))

  def resource(name: String, warnTimeouts: IterableOnce[FiniteDuration] = defaultWarnTimeouts)
  : Resource[Task, Unit] = {
    val lock = MVar[Task].of(()).memoize

    val acquire = lock
      .flatMap(mvar =>
        mvar.tryTake
          .flatMap {
            case None =>
              scribe.debug(s"Waiting for '$name' lock")
              mvar.take
                .whenItTakesLonger(warnTimeouts)(duration =>
                  scribe.info(s"Still waiting for '$name' lock for ${duration.pretty} ..."))
            case Some(()) =>
              scribe.trace(s"Lock '$name' acquired")
              Task.unit
          })

    val release =
      Task(scribe.trace(s"Lock '$name' released")) >>
        lock.flatMap(_.put(()))

    Resource.make(acquire)(_ => release)
  }
}

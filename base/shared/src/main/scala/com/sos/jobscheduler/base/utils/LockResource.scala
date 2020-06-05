package js7.base.utils

import cats.effect.Resource
import monix.catnap.MVar
import monix.eval.Task

object LockResource
{
  def apply(): Resource[Task, Unit] = {
    val lock = MVar[Task].of(()).memoize
    Resource.make(lock.flatMap(_.take))(_ => lock.flatMap(_.put(())))
  }
}

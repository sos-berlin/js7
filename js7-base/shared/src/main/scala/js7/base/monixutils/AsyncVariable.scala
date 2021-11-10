package js7.base.monixutils

import js7.base.problem.Checked
import js7.base.utils.AsyncLock
import monix.eval.Task

final class AsyncVariable[V](initial: V)
{
  @volatile private var _value = initial
  private val lock = AsyncLock("AsyncVariable")

  def get: V =
    _value

  def updateCheckedWithResult[R](update: V => Task[Checked[(V, R)]]): Task[Checked[R]] =
    lock.lock(
      update(_value)
        .map(_.map { case (v, r) =>
          _value = v
          r
        }))
}

object AsyncVariable
{
  def apply[A](initial: A) =
    new AsyncVariable(initial)
}

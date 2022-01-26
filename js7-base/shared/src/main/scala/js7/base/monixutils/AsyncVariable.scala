package js7.base.monixutils

import js7.base.problem.Checked
import js7.base.utils.AsyncLock
import monix.eval.Task

// TODO Equivalent to MVar?
final class AsyncVariable[V](initial: V)
{
  @volatile private var _value = initial
  private val lock = AsyncLock("AsyncVariable")

  def get: V =
    _value

  def update(update: V => Task[V]): Task[V] =
    shieldValue(
      for (v <- update(_value)) yield {
        _value = v
        v
      })

  def updateChecked(update: V => Task[Checked[V]]): Task[Checked[V]] =
    shieldValue(
      for (checked <- update(_value)) yield {
        for (v <- checked) _value = v
        checked
      })

  def updateWithResult[R](update: V => Task[(V, R)]): Task[R] =
    shieldValue(
      update(_value)
      .map { case (v, r) =>
        _value = v
        r
      })

  def updateCheckedWithResult[R](update: V => Task[Checked[(V, R)]]): Task[Checked[R]] =
    shieldValue(
      update(_value)
      .map(_.map { case (v, r) =>
        _value = v
        r
      }))

  private def shieldValue[A](body: => Task[A]): Task[A] =
    lock.lock(Task.defer/*shield access to _value in body*/(body))
}

object AsyncVariable
{
  def apply[A](initial: A) =
    new AsyncVariable(initial)
}

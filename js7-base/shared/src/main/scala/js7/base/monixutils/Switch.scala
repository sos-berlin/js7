package js7.base.monixutils

import monix.catnap.MVar
import monix.eval.Task
import monix.reactive.Observable

final class Switch private(initiallyOn: Boolean)
{
  private val switch = (if (initiallyOn) MVar[Task].empty[Unit]() else MVar[Task].of(())).memoize

  /** Returns true iff switch turned from off to on. */
  val switchOn: Task[Boolean] =
    switch.flatMap(_.tryTake).map(_.nonEmpty)

  def switchOnThen(task: => Task[Unit]): Task[Unit] =
    switchOn.flatMap(Task.when(_)(task))

  /** Returns true iff switch turned from on to off. */
  val switchOff: Task[Boolean] =
    switch.flatMap(_.tryPut(()))

  def switchOffThen(task: => Task[Unit]): Task[Unit] =
    switchOff.flatMap(Task.when(_)(task))

  val isOn: Task[Boolean] =
    switch.flatMap(_.isEmpty)

  val isOff: Task[Boolean] =
    isOn.map(!_)

  val whenOff: Task[Unit] =
    switch.flatMap(_.read).void

  def whenOffObservable: Observable[Unit] =
    Observable.fromTask(whenOff)
}

object Switch {
  def apply(on: Boolean) = new Switch(on)
}

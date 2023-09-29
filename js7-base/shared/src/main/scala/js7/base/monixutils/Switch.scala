package js7.base.monixutils

import cats.syntax.flatMap.*
import monix.catnap.MVar
import monix.eval.Task

final class Switch private(initiallyOn: Boolean)
extends Switch.ReadOnly:
  private val lock = new SimpleLock

  private val filledWhenOff: Task[MVar[Task, Unit]] =
    (if initiallyOn then MVar[Task].empty[Unit]() else MVar[Task].of(()))
      .memoize
  private val filledWhenOn: Task[MVar[Task, Unit]] =
    (if !initiallyOn then MVar[Task].empty[Unit]() else MVar[Task].of(()))
      .memoize

  /** Returns true iff switch turned from off to on. */
  val switchOn: Task[Boolean] =
    lock.lock(
      filledWhenOff
        .flatMap(_.tryTake)
        .flatTap(_ => filledWhenOn.flatMap(_.tryPut(())))
        .map(_.nonEmpty))

  // Not nestable !!!
  def switchOnFor[A](task: Task[A]): Task[A] =
    switchOn *> task.guarantee(switchOff.void)

  /** Switch on and return `task` iff switch was previously off. */
  def switchOnThen(task: => Task[Unit]): Task[Unit] =
    switchOn.flatMap(Task.when(_)(task))

  /** Returns true iff switch turned from on to off. */
  val switchOff: Task[Boolean] =
    lock.lock(
      filledWhenOff
        .flatMap(_.tryPut(()))
        .flatTap(_ => filledWhenOn.flatMap(_.tryTake)))

  def switchOffThen(task: => Task[Unit]): Task[Unit] =
    switchOff.flatMap(Task.when(_)(task))

  val isOn: Task[Boolean] =
    filledWhenOff.flatMap(_.isEmpty)

  val isOff: Task[Boolean] =
    isOn.map(!_)

  val whenOff: Task[Unit] =
    filledWhenOff.flatMap(_.read).void

  val whenOn: Task[Unit] =
    filledWhenOn.flatMap(_.read).void

object Switch:
  def apply(on: Boolean) = new Switch(on)

  trait ReadOnly:
    def isOn: Task[Boolean]

    def isOff: Task[Boolean]

    def whenOff: Task[Unit]

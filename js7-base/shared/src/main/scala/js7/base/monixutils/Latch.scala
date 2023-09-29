package js7.base.monixutils

import monix.catnap.MVar
import monix.eval.Task

/** A single-use latch, after being closed it can never opened again. */
final class Latch extends Latch.ReadOnly:
  // Initially open
  private val latch = MVar[Task].empty[Unit]().memoize

  val is: Task[Boolean] =
    latch.flatMap(_.isEmpty)

  val isNot: Task[Boolean] =
    is.map(!_)

  /** Close the latch and return true if not was already closed. */
  val switch: Task[Boolean] =
    latch.flatMap(_.tryPut(()))

  /** Close and return `task` iff switch was previously off. */
  def switchThen[A](task: => Task[A]): Task[Option[A]] =
    switch.flatMap(hasSwitched =>
      if hasSwitched then
        task.map(Some(_))
      else
        Task.none)

  val when: Task[Unit] =
    latch.flatMap(_.read).void

object Latch:
  trait ReadOnly:
    def is: Task[Boolean]

    def isNot: Task[Boolean]

    def when: Task[Unit]

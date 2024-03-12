package js7.base.monixutils

import cats.effect.IO
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.utils.MVar

final class Switch private(initiallyOn: Boolean)
extends Switch.ReadOnly:
  private val lock = SimpleLock[IO]

  private val filledWhenOff: IO[MVar[IO, Unit]] =
    memoize:
      if initiallyOn then MVar[IO].empty[Unit] else MVar[IO].of(())

  private val filledWhenOn: IO[MVar[IO, Unit]] =
    memoize:
      if !initiallyOn then MVar[IO].empty[Unit] else MVar[IO].of(())

  /** Returns true iff switch turned from off to on. */
  val switchOn: IO[Boolean] =
    lock.surround:
      filledWhenOff
        .flatMap(_.tryTake)
        .flatTap(_ => filledWhenOn.flatMap(_.tryPut(())))
        .map(_.nonEmpty)

  // Not nestable !!!
  def switchOnAround[A](body: IO[A]): IO[A] =
    switchOn *> body.guarantee(switchOff.void)

  /** Switch on and return `task` iff switch was previously off. */
  def switchOnThen(body: => IO[Unit]): IO[Unit] =
    switchOn.flatMap(IO.whenA(_)(body))

  /** Returns true iff switch turned from on to off. */
  val switchOff: IO[Boolean] =
    lock.surround:
      filledWhenOff
        .flatMap(_.tryPut(()))
        .flatTap(_ => filledWhenOn.flatMap(_.tryTake))

  def switchOffThen(body: => IO[Unit]): IO[Unit] =
    switchOff.flatMap(IO.whenA(_)(body))

  val isOn: IO[Boolean] =
    filledWhenOff.flatMap(_.isEmpty)

  val isOff: IO[Boolean] =
    isOn.map(!_)

  val whenOff: IO[Unit] =
    filledWhenOff.flatMap(_.read).void

  val whenOn: IO[Unit] =
    filledWhenOn.flatMap(_.read).void


object Switch:
  def apply(on: Boolean) = new Switch(on)

  trait ReadOnly:
    def isOn: IO[Boolean]

    def isOff: IO[Boolean]

    def whenOff: IO[Unit]

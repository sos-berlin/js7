package js7.base.monixutils

import cats.effect.IO
import fs2.concurrent.SignallingRef
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.fs2utils.StreamExtensions.+:

final class Switch private(initiallyOn: Boolean)
extends Switch.ReadOnly:

  private val ref = memoize(SignallingRef[IO].of(initiallyOn))

  /** Returns true iff switch turned from off to on. */
  val switchOn: IO[Boolean] =
    ref.flatMap: ref =>
      ref.modify(true -> !_)

  /** Returns true iff switch turned from on to off. */
  val switchOff: IO[Boolean] =
    ref.flatMap: ref =>
      ref.modify(false -> _)

  // Not nestable !!!
  def switchOnAround[A](body: IO[A]): IO[A] =
    switchOn *> body.guarantee(switchOff.void)

  /** Switch on and return `task` iff switch was previously off. */
  def switchOnThen(body: => IO[Unit]): IO[Unit] =
    switchOn.flatMap(IO.whenA(_)(body))

  def switchOffThen(body: => IO[Unit]): IO[Unit] =
    switchOff.flatMap(IO.whenA(_)(body))

  val isOn: IO[Boolean] =
    ref.flatMap(_.get)

  val isOff: IO[Boolean] =
    isOn.map(!_)

  val whenOn: IO[Unit] =
    when(true)

  val whenOff: IO[Unit] =
    when(false)

  def when(predicate: Boolean): IO[Unit] =
    ref.flatMap:
      _.getAndDiscreteUpdates.use: (o, stream) =>
        (o +: stream).takeThrough(_ == !predicate).compile.last.map(_.isDefined)


object Switch:
  def apply(on: Boolean) = new Switch(on)

  trait ReadOnly:
    def isOn: IO[Boolean]

    def isOff: IO[Boolean]

    def whenOn: IO[Unit]

    def whenOff: IO[Unit]

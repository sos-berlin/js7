package js7.base.monixutils

import cats.effect.std.Queue
import cats.effect.{Deferred, IO}
import cats.syntax.flatMap.*
import js7.base.catsutils.UnsafeMemoizable
import js7.base.catsutils.UnsafeMemoizable.given
import js7.base.catsutils.UnsafeMemoizable.*
import js7.base.utils.{Atomic, MVar}

final class Switch(initiallyOn: Boolean)
extends Switch.ReadOnly:

  private val on = Atomic(initiallyOn)
  private val lock = SimpleLock[IO]

  private val onQueue = Queue.bounded[IO, Unit](1)
    .flatMap(q => IO.whenA(initiallyOn)(q.offer(())).as(q)).unsafeMemoize

  private val offQueue = Queue.bounded[IO, Unit](1)
    .flatMap(q => IO.unlessA(initiallyOn)(q.offer(())).as(q)).unsafeMemoize

  private val filledWhenOff: IO[MVar[IO, Unit]] =
    (if initiallyOn then MVar[IO].empty[Unit] else MVar[IO].of(()))
      .unsafeMemoize

  private val filledWhenOn: IO[MVar[IO, Unit]] =
    (if !initiallyOn then MVar[IO].empty[Unit] else MVar[IO].of(()))
      .unsafeMemoize

  /** Returns true iff switch turned from off to on. */
  val switchOn: IO[Boolean] =
    lock.surround:
      if on.getAndSet(true) then
        IO.pure(false)
      else
        for
          _ <- offQueue.flatMap(_.take)
          _ <- onQueue.flatMap(_.offer(()))
        yield true

  // Not nestable !!!
  def switchOnAround[A](io: IO[A]): IO[A] =
    switchOn *> io.guarantee(switchOff.void)

  /** Switch on and return `io` iff switch was previously off. */
  def switchOnThen(io: => IO[Unit]): IO[Unit] =
    switchOn.flatMap(IO.whenA(_)(io))

  /** Returns true iff switch turned from on to off. */
  val switchOff: IO[Boolean] =
    lock.surround:
      filledWhenOff
        .flatMap(_.tryPut(()))
        .flatTap(_ => filledWhenOn.flatMap(_.tryTake))

  def switchOffThen(io: => IO[Unit]): IO[Unit] =
    switchOff.flatMap(IO.whenA(_)(io))

  val isOn: IO[Boolean] =
    filledWhenOff.flatMap(_.isEmpty)

  val isOff: IO[Boolean] =
    isOn.map(!_)

  val whenOff: IO[Unit] =
    filledWhenOff.flatMap(_.read).void

  val whenOn: IO[Unit] =
    filledWhenOn.flatMap(_.read).void


object Switch:
  trait ReadOnly:
    def isOn: IO[Boolean]

    def isOff: IO[Boolean]

    def whenOff: IO[Unit]

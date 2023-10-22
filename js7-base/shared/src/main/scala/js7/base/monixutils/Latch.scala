package js7.base.monixutils

import cats.effect.std.{Mutex, Queue}
import cats.effect.{Deferred, IO, Ref}
import js7.base.catsutils.UnsafeMemoizable.given
import js7.base.utils.Atomic.extensions.:=
import js7.base.utils.{Atomic, MVar}

/** A single-use, initially open latch.
 * After being closed it cannot opened again. */
final class Latch extends Latch.ReadOnly:

  private val deferred = Deferred.unsafe[IO, Unit]

  /** True before switch. */
  val isNot: IO[Boolean] =
    deferred.tryGet.map(_.isEmpty)

  /** True after switch. */
  val is: IO[Boolean] =
    deferred.tryGet.map(_.isDefined)

  /** Close the latch and return true if not was already closed. */
  val switch: IO[Boolean] =
    deferred.complete(())

  /** Close and return `io` iff switch was previously off. */
  def switchThen[A](io: => IO[A]): IO[Option[A]] =
    switch.flatMap(hasSwitched =>
      if hasSwitched then
        io.map(Some(_))
      else
        IO.none)

  def when: IO[Unit] =
    deferred.get


object Latch:
  trait ReadOnly:
    def is: IO[Boolean]

    def isNot: IO[Boolean]

    def when: IO[Unit]

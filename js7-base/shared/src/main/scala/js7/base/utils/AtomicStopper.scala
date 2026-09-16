package js7.base.utils

import cats.effect.std.{AtomicCell, Mutex}
import cats.effect.{IO, Resource, ResourceIO}

final class AtomicStopper private(cell: AtomicCell[IO, Boolean], mutex: Mutex[IO]):

  def stop: IO[Unit] =
    mutex.lock.surround:
      cell.set(true)

  def peek: IO[Boolean] =
    cell.get

  val resource: ResourceIO[Boolean] =
    for
      _ <- mutex.lock
      stopped <- Resource.eval(cell.get)
    yield
      stopped


object AtomicStopper:
  def apply(): IO[AtomicStopper] =
    for
      cell <- AtomicCell[IO].of(false)
      mutex <- Mutex[IO]
    yield
      new AtomicStopper(cell, mutex)

package js7.base.utils

import cats.effect.std.Mutex
import cats.effect.{IO, Ref, Resource, ResourceIO}
import cats.syntax.option.*

final class AtomicStopper private(
  ref: Ref[IO, Option[String]],
  mutex: Mutex[IO],
  label: String):

  /** Atomically mark as stopped.
    */
  def stop(reason: String): IO[Unit] =
    mutex.lock.surround:
      ref.set(Some(reason))

  def peek: IO[Boolean] =
    ref.get.map(_.isDefined)

  /** The Resource provides a Some(reason: String) iff stopped.
    *
    * The AtomicStopper cannot be stopped while the resource is in use.
    */
  val resource: ResourceIO[Option[String]] =
    for
      _ <- mutex.lock
      stopReason <- Resource.eval(ref.get)
    yield
      stopReason

  override def toString = s"AtomicStopper($label)"


object AtomicStopper:

  def apply(label: String = ""): IO[AtomicStopper] =
    for
      cell <- Ref[IO].of(none[String])
      mutex <- Mutex[IO]
    yield
      new AtomicStopper(cell, mutex, label)

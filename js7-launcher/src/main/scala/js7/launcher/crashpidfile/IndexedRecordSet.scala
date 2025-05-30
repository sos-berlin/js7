package js7.launcher.crashpidfile

import cats.effect.kernel.Resource
import cats.effect.{IO, ResourceIO}
import java.nio.file.Path

trait IndexedRecordSet[A]:

  /** Adds and removes  an A. */
  def register(a: A): ResourceIO[Unit]

  /** Explicit removal, in case of auto-removal via `register` is not enough. */
  def remove(a: A): IO[Unit]


object IndexedRecordSet:

  export WriteBasedIndexedRecordSet.{file, textFile}

  def dummy[A]: Dummy[A] =
    Dummy.asInstanceOf[Dummy[A]]


  trait Dummy[A] extends IndexedRecordSet[A]:
    final def register(a: A): ResourceIO[Unit] =
      Resource.unit

    final def remove(a: A): IO[Unit] =
      IO.unit

    override def toString = s"IndexedRecordSet:Dummy"

  private object Dummy extends Dummy[Any]

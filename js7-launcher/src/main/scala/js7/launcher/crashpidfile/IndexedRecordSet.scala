package js7.launcher.crashpidfile

import cats.effect.ResourceIO
import cats.effect.kernel.Resource
import java.nio.ByteBuffer
import java.nio.file.Path
import js7.base.utils.ScalaUtils.*

trait IndexedRecordSet[A]:
  def register(a: A): ResourceIO[Unit]


object IndexedRecordSet:

  export IndexedRecordSetImpl.{apply,file}

  // TODO Use export
  // Because Scala 3.3.3 export looses the default argument:
  def textFile[A](path: Path, stringByteSize: Int, label: String = "")
    (writeBuffer: (ByteBuffer, A) => Unit)
  : ResourceIO[IndexedRecordSetImpl[A]] =
    IndexedRecordSetImpl.textFile(path, stringByteSize, label)(writeBuffer)


  def dummy[A]: Dummy[A] =
    Dummy.asInstanceOf[Dummy[A]]

  trait Dummy[A] extends IndexedRecordSet[A]:
    final def register(a: A): ResourceIO[Unit] =
      Resource.unit

    override def toString = s"IndexedRecordSet:Dummy"

  private object Dummy extends Dummy[Any]

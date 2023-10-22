package js7.base.utils

import cats.effect.Resource
import izumi.reflect.Tag
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.utils.CatsUtils.syntax.RichResource
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import scala.concurrent.duration.*
import scala.util.Try

final case class ResourceAutoCloseable[A](resource: Resource[IO, A], timeout: Duration)
  (using Tag[A], IORuntime, sourcecode.Enclosing)
extends AutoCloseable:

  private lazy val allocated = Try(resource.toAllocated.await(timeout))
  private val closed = Atomic(false)

  def value: A =
    allocated.get.allocatedThing

  def close(): Unit =
    if !closed.getAndSet(true) then
      for a <- allocated do a.release.await(timeout)

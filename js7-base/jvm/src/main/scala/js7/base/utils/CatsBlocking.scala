package js7.base.utils

import cats.effect.Resource
import izumi.reflect.Tag
import js7.base.utils.AutoClosing.autoClosing
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*

object CatsBlocking:

  implicit final class BlockingIOResource[A](private val resource: Resource[IO, A]) extends AnyVal:
    @TestOnly
    def blockingUse[R](timeout: Duration = Duration.Inf)(block: A => R)
      (implicit aTag: Tag[A], rt: IORuntime, src: sourcecode.Enclosing)
    : R =
      autoClosing(ResourceAutoCloseable(resource, timeout))(o =>
        block(o.value))

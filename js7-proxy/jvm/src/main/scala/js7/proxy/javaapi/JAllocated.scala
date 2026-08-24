package js7.proxy.javaapi

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import izumi.reflect.Tag
import java.util.concurrent.CompletableFuture
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.utils.Allocated
import js7.proxy.javaapi.JAllocated.*

/** An allocated resource wjich must be released by the caller. */
final class JAllocated[A: Tag as aTag] private[javaapi](asScala: Allocated[IO, A])
  (using IORuntime):

  def allocatedThing: A =
    asScala.allocatedThing

  def release: CompletableFuture[Void] =
    logger
      .traceIO(s"JAllocated[${aTag.tag}] release"):
        asScala.release.as(null.asInstanceOf[Void])
      .unsafeToCompletableFuture()


private object JAllocated:
  private val logger = Logger[this.type]

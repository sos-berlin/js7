package js7.proxy.javaapi

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import izumi.reflect.Tag
import java.util.concurrent.CompletableFuture
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.data_for_java.common.JavaUtils.-->
import js7.proxy.javaapi.JAllocated.*

// NOT USED. See safer JResource!
private final class JAllocated[A: Tag as aTag] private(asScala: Allocated[IO, A])(using IORuntime):

  def allocatedThing: A =
    asScala.allocatedThing

  def release(): CompletableFuture[Void] =
    release(None)

  private def release(throwable: Option[Throwable]): CompletableFuture[Void] =
    logger
      .traceIO(
        s"JAllocated[${aTag.tag}] release",
        throwable.map(t => "💥 " + t.toStringWithCauses) getOrElse ""
      ):
        asScala.release.as(null.asInstanceOf[Void])
      .unsafeToCompletableFuture()

  def use[R](body: A --> CompletableFuture[R]): CompletableFuture[R] =
    body(allocatedThing)
      .thenCompose: b =>
        release().thenApply((_: Void) => b)
      .exceptionallyCompose: throwable =>
        release(Some(throwable)).thenCompose: (_: Void) =>
          CompletableFuture.failedFuture(throwable)


private object JAllocated:
  private val logger = Logger[this.type]

  def apply[A: Tag as aTag](resource: ResourceIO[A])(using IORuntime): IO[JAllocated[A]] =
    logger.traceIO(s"JAllocated[${aTag.tag}] allocate"):
      resource.toAllocated.map(new JAllocated(_))

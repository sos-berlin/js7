package js7.proxy.javaapi

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import izumi.reflect.Tag
import java.util.concurrent.CompletableFuture
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.data_for_java.common.JavaUtils.-->
import js7.proxy.javaapi.JResource.*

final class JResource[A: Tag as aTag](asScala: ResourceIO[A])(using IORuntime):

  /** Use the resource and then release it.
    *
    * The resource is released even in case of an error. */
  def use[R](body: A --> CompletableFuture[R]): CompletableFuture[R] =
    logger.traceIO(s"JResource[${aTag.tag}] use"):
      asScala.use: a =>
        IO.fromCompletableFuture:
          IO:
            body(a)
    .unsafeToCompletableFuture()

  /** Return the resource and a release function.
    *
    * ⚠️ The caller must call the release function after use.
    *
    * Prefer [[use]] which automatically releases the resource!
    */
  def allocate: CompletableFuture[JAllocated[A]] =
    asScala.toAllocated.map(JAllocated(_)).unsafeToCompletableFuture()


object JResource:
  private val logger = Logger[this.type]

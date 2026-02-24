package js7.proxy.javaapi

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import izumi.reflect.Tag
import java.util.concurrent.{CompletableFuture, Executor, ForkJoinPool}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.proxy.javaapi.JResource.*

final class JResource[A: Tag as aTag](asScala: ResourceIO[A])(using IORuntime):

  def use[R](body: java.util.function.Function[A, CompletableFuture[R]]): CompletableFuture[R] =
    logger.traceIO(s"JResource[${aTag.tag}] use"):
      asScala.use: a =>
        IO.fromCompletableFuture:
          IO:
              body(a)
          .evalOnExecutor(ourCommonPool)
    .unsafeToCompletableFuture()

object JResource:
  private val logger = Logger[this.type]

  /** The believed `Executor` of the Java caller. */
  def ourCommonPool: Executor =
    ForkJoinPool.commonPool

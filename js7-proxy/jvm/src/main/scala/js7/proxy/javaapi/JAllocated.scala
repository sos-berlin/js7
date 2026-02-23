package js7.proxy.javaapi

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.util.concurrent.CompletionStage
import js7.base.utils.Allocated

final class JAllocated[A](asScala: Allocated[IO, A])(using IORuntime):

  def allocatedThing: A =
    asScala.allocatedThing

  def release: CompletionStage[Void] =
    asScala.release.as(null.asInstanceOf[Void]).unsafeToCompletableFuture()

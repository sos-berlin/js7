package js7.executor.forjava.internal

import java.util.concurrent.{CompletableFuture, CompletionStage}
import javax.annotation.Nonnull
import js7.data_for_java.common.JavaUtils.Void
import js7.executor.internal.InternalJobAdapter

@InternalJobAdapter(classOf[JInternalJobAdapter])
trait JInternalJob
{
  @Nonnull
  def start: CompletionStage[Void] =
    CompletableFuture.completedFuture(Void)
    // Since Java 9: CompletableFuture.completedStage(Void)

  /** Reserved. */
  @Nonnull
  final def stop: CompletionStage[Void] =
    CompletableFuture.completedFuture(Void)

  @Nonnull
  def processOrder(@Nonnull orderContext: JOrderContext): JOrderProcess
}

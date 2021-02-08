package js7.executor.forjava.internal

import javax.annotation.Nonnull
import js7.executor.internal.InternalJobAdapter

/** Simplifys implementation by calling the methods in commonPool. */
@InternalJobAdapter(classOf[BlockingInternalJobAdapter])
trait BlockingInternalJob
{
  /** Reserved. */
  final def start() = {}

  /** Reserved. */
  final def stop() = {}

  /** Process the order in a seperate thread. */
  @Nonnull
  def processOrder(@Nonnull context: JOrderContext): JOrderResult
}

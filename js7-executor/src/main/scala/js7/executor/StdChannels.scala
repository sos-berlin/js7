package js7.executor

import monix.execution.atomic.Atomic
import monix.reactive.Observer

final case class StdChannels(
  out: Observer[String],
  err: Observer[String],
  charBufferSize: Int)
extends AutoCloseable
{
  private final val closed = Atomic(false)

  def close() =
    if (!closed.getAndSet(true)) {
      try out.onComplete()
      finally err.onComplete()
    }
}

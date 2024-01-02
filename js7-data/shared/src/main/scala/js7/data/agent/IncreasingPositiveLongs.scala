package js7.data.agent

import java.util.concurrent.atomic.AtomicLong
import java.util.function.LongUnaryOperator
import scala.collection.AbstractIterator

/**
 * Delivers thread-safe and strictly increasing Ints, wrapping from Long.MaxValue to 1.
 *
 * @author Joacim Zschimmer
 */
final class IncreasingPositiveLongs(start: Long = 1, maximum: Long = Long.MaxValue)
extends AbstractIterator[Long]:

  private val counter = new AtomicLong(start)

  def hasNext = true

  def next() =
    counter.getAndUpdate(new LongUnaryOperator {
      def applyAsLong(n: Long) =
        n match {
          case `maximum` => 1
          case _ => n + 1
        }
    })

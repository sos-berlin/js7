package js7.common.utils

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class UntilNoneIterator[A] private(read: => Option[A])
extends com.google.common.collect.AbstractIterator[A]
with Iterator[A]
{
  def computeNext() = read getOrElse endOfData
}

object UntilNoneIterator
{
  def apply[A](read: => Option[A]): Iterator[A] =
    new UntilNoneIterator(
      try read map Success.apply
      catch {
        case NonFatal(t) => Some(Failure(t))
      }
    ) map (_.get/*throws*/)
}

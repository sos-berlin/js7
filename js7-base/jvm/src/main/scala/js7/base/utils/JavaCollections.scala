package js7.base.utils

import scala.jdk.CollectionConverters.*

/**
  * @author Joacim Zschimmer
  */
object JavaCollections
{
  object syntax
  {
    implicit final class JavaStreamToScala[A](private val underlying: java.util.stream.Stream[A]) extends AnyVal {
      def asScala: Iterator[A] = underlying.iterator.asScala
    }
  }
}

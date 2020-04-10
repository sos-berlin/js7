package com.sos.jobscheduler.base.utils

import scala.jdk.CollectionConverters._
/**
  * @author Joacim Zschimmer
  */
object JavaCollections
{
  object syntax
  {
    implicit final class JavaStreamToScala[A](private val underlying: java.util.stream.Stream[A]) extends AnyVal {
      def asScala: Iterator[A] = underlying.iterator.asScala

      def toImmutableSeq: Seq[A] =
        Vector.empty ++ underlying.iterator.asScala
    }
  }
}


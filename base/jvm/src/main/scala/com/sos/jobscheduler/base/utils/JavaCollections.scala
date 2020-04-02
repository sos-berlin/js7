package com.sos.jobscheduler.base.utils

import scala.collection.JavaConverters._
import scala.collection.immutable.{Seq, Vector}
import scala.language.implicitConversions

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


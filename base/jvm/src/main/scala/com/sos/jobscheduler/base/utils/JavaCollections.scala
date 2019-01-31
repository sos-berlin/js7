package com.sos.jobscheduler.base.utils

import scala.collection.JavaConverters._
import scala.collection.immutable.{Seq, Vector}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object JavaCollections
{
  object implicits {
    implicit def javaStreamToIterator[A](stream: java.util.stream.Stream[A]): Iterator[A] = stream.iterator.asScala

    implicit final class RichJavaStream[A](private val delegate: java.util.stream.Stream[A]) extends AnyVal {
      def toImmutableSeq: Seq[A] =
        Vector.empty ++ delegate.iterator.asScala
    }
  }
}


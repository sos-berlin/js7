package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.JavaCollections.implicits._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.immutable.Iterable

/**
  * @author Joacim Zschimmer
  */
final class JavaCollectionsTest extends FreeSpec
{
  "java.util.stream.Stream.toImmutableSeq" in {
    java.util.stream.Stream.of(1, 2, 3).toImmutableSeq shouldEqual List(1, 2, 3)
  }

  "java.util.stream.Stream.toVector" in {
    java.util.stream.Stream.of(1, 2, 3).toVector shouldEqual Vector(1, 2, 3)
  }

  "java.util.stream.Stream.toSet" in {
    java.util.stream.Stream.of(1, 2, 2, 3).toSet shouldEqual Set(1, 2, 3)
  }

  "java.util.stream.Stream.toIterator" in {
    for ((a, b) ← java.util.stream.Stream.of(1, 2, 3).toIterator zip Iterator(1, 2, 3))
      assert(a == b)
  }

  "java.util.stream.Stream.toIterable" in {
    assert(java.util.stream.Stream.of(1, 2, 3).toIterable == Iterable(1, 2, 3))
  }

  "java.util.stream.Stream.toStream" in {
    assert(java.util.stream.Stream.of(1, 2, 3).toStream == Stream(1, 2, 3))
  }
}

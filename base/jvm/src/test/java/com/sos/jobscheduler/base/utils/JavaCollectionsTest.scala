package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.JavaCollections.syntax._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaCollectionsTest extends FreeSpec
{
  "java.util.stream.Stream.asScala" in {
    for ((a, b) <- java.util.stream.Stream.of(1, 2, 3).asScala zip Iterator(1, 2, 3))
      assert(a == b)
  }
}

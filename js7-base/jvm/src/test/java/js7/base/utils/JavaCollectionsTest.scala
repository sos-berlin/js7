package js7.base.utils

import js7.base.utils.JavaCollections.syntax.*
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaCollectionsTest extends AnyFreeSpec
{
  "java.util.stream.Stream.asScala" in {
    for ((a, b) <- java.util.stream.Stream.of(1, 2, 3).asScala zip Iterator(1, 2, 3))
      assert(a == b)
  }
}

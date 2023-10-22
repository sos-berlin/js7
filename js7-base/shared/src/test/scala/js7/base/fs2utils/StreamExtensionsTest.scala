package js7.base.fs2utils

import fs2.Stream
import js7.base.fs2utils.StreamExtensions.*
import js7.base.test.OurTestSuite

final class StreamExtensionsTest extends OurTestSuite:

  "+:" in:
    val intStream: Stream[Pure, Int] = 1 +: Stream(2, 3)
    assert(intStream.toSeq == Seq(1, 2, 3))

    assert(("one" +: Stream[Int](2, 3)).toSeq == Seq("one", 2, 3))

  "prepend" in:
    val intStream: Stream[Pure, Int] = Stream(2, 3).prepend(1)
    assert(intStream.toSeq == Seq(1, 2, 3))

    assert((Stream(2, 3).prepend("one")).toSeq == Seq("one", 2, 3))

  "takeUntilEval" in:
    ???

package js7.base.fs2utils

import fs2.{Pure, Stream}
import js7.base.fs2utils.StreamExtensions.*
import js7.base.test.OurTestSuite

final class StreamExtensionsTest extends OurTestSuite:

  "+:" in:
    val intStream: Stream[Pure, Int] = 1 +: Stream(2, 3)
    assert(intStream.compile.to(Seq) == Seq(1, 2, 3))

    val stream = ("one" +: Stream[Pure, Int](2, 3))
    assert(stream.toList == List("one", 2, 3))

  "prepend" in:
    val intStream: Stream[Pure, Int] = Stream(2, 3).prepend(1)
    assert(intStream.compile.to(Seq) == Seq(1, 2, 3))

    val anyStream = Stream(2, 3).prepend("one")
    assert(anyStream.toList == List("one", 2, 3))

  "takeUntilEval" in:
    pending // FIXME Monix

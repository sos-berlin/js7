package js7.common.pekkohttp

import fs2.Stream
import js7.base.fs2utils.StreamExtensions.*
import js7.base.test.OurAsyncTestSuite
import js7.common.pekkohttp.ByteSequenceStreamExtensions.*
import js7.common.pekkoutils.ByteStrings.syntax.*
import org.apache.pekko.util.ByteString
import scala.util.Random

final class ByteSequenceStreamExtensionsTest extends OurAsyncTestSuite:

  "splitByteSequences" in:
    val maxSize = 2
    val strings = Vector.fill(100_000)(
      ('A' + Random.nextInt(26)).toChar.toString * Random.nextInt(maxSize + 1) + "\n")
    val expectedCount = strings.view.map(o => (o.size + maxSize - 1) / maxSize).sum

    Stream
      .iterable(strings)
      .map(ByteString(_))
      .splitByteSequences(maxSize)
      .tapEach: byteString =>
        assert(byteString.length <= maxSize)
      .compile.toList
      .map: result =>
        assert(result.length == expectedCount)
        assert(result.map(_.utf8String).mkString == strings.mkString)

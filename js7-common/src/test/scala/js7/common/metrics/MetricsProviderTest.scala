package js7.common.metrics

import java.nio.charset.StandardCharsets.UTF_8
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.test.OurAsyncTestSuite
import js7.common.pekkoutils.ByteStrings.syntax.ByteStringToByteSequence

final class MetricsProviderTest extends OurAsyncTestSuite:

  "splitMeasurements" in:
    val input =
      """# HELP
        |#X
        |data 1
        |# HELP
        |data 2
        |data 3
        |data 4
        |# HELP
        |data 5
        |data 6
        |""".stripMargin

    val measurements =
      fs2.Stream.chunk:
        fs2.Chunk.array(input.getBytes(UTF_8))
      .chunkN(20)
      .map(_.toByteString)
      .through:
        MetricsProvider.splitMeasurements[fs2.Pure]
      .map:
        _.utf8String
      .compile.toList

    assert(measurements == List(
      """# HELP
        |#X
        |data 1
        |""".stripMargin,
      """# HELP
        |data 2
        |data 3
        |data 4
        |""".stripMargin,
      """# HELP
        |data 5
        |data 6
        |""".stripMargin))

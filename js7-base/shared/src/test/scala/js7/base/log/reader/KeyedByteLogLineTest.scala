package js7.base.log.reader

import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import js7.base.problem.Problem
import js7.base.test.OurTestSuite

final class KeyedByteLogLineTest extends OurTestSuite:

  private val keyedByteLogLine = KeyedByteLogLine(
    fileInstant = Instant.parse("2026-04-30T00:00:00.12345Z"),
    PosAndLine(
      1112223334445556667L,
      toChunk("LINE\n")))

  "asByteSeq, parse" in:
    val chunk = toChunk("1777507200.12345/1112223334445556667 LINE\n")
    assert(keyedByteLogLine.asByteSeq == chunk)
    assert(KeyedByteLogLine.parse(chunk) == Right(keyedByteLogLine))
    assert(KeyedByteLogLine.parse(toChunk("Info/1/2")) == Left(Problem:
      "Invalid KeyedByteLogLine format"))

    val legacyByteString = toChunk("Info/1777507200.12345/1112223334445556667 LINE\n")
    assert(KeyedByteLogLine.parse(legacyByteString) == Right(keyedByteLogLine))


  private def toChunk(string: String) =
    fs2.Chunk.array(string.getBytes(UTF_8))

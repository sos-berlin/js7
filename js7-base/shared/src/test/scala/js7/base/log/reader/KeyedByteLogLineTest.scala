package js7.base.log.reader

import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import js7.base.log.LogLevel.Info
import js7.base.problem.Problem
import js7.base.test.OurTestSuite

final class KeyedByteLogLineTest extends OurTestSuite:

  private val keyedByteLogLine = KeyedByteLogLine(
    logLevel = Info,
    fileInstant = Instant.parse("2026-04-30T00:00:00Z"),
    PosAndLine(
      1112223334445556667L,
      toChunk("LINE\n")))

  "asString, parse" in:
    val byteString = toChunk("Info/1777507200/1112223334445556667 LINE\n")
    assert(keyedByteLogLine.asByteSeq == byteString)
    assert(KeyedByteLogLine.parse(byteString) == Right(keyedByteLogLine))
    assert(KeyedByteLogLine.parse(toChunk("Info/1/2")) == Left(Problem("Invalid KeyedByteLogLine format")))


  private def toChunk(string: String) =
    fs2.Chunk.array(string.getBytes(UTF_8))

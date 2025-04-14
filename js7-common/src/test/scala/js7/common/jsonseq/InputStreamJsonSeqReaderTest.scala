package js7.common.jsonseq

import io.circe.Json
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets.{US_ASCII, UTF_8}
import js7.base.io.file.FileUtils.withTemporaryFile
import js7.base.test.OurTestSuite
import js7.base.utils.Ascii.LF
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.jsonseq.InputStreamJsonSeqReaderTest.*
import js7.common.message.ProblemCodeMessages
import org.scalatest.matchers.should.Matchers.*

/**
  * @author Joacim Zschimmer
  */
final class InputStreamJsonSeqReaderTest extends OurTestSuite:

  ProblemCodeMessages.initialize()

  "Empty file" in:
    val in = new InputStream with SeekableInputStream:
      def read() = -1
      def seek(pos: Long) = throw new NotImplementedError
    val reader = newInputStreamJsonSeqReader(in)
    assert(reader.read() == None)

  "read, seek" in:
    var seekCount = 0
    val in = new InputStream with SeekableInputStream:
      var pos = 0
      def read(): Int =
        if pos == ChunkBytes.length then
          -1
        else
          pos += 1
          ChunkBytes(pos - 1)
      def seek(pos: Long) =
        seekCount += 1
        this.pos = pos.toInt
    val reader = newInputStreamJsonSeqReader(in, blockSize = 4)
    assert(reader.read() == Some(Chunk(0)._2))
    assert(reader.read() == Some(Chunk(1)._2))
    assert(reader.read() == Some(Chunk(2)._2))
    assert(reader.read() == Some(Chunk(3)._2))
    assert(reader.read() == None)
    assert(seekCount == 0)
    assert(reader.read() == None)

    // seek
    reader.seek(2+3)
    assert(seekCount == 1)
    assert(reader.read() == Some(Chunk(2)._2))
    reader.seek(0)
    assert(seekCount == 2)
    assert(reader.read() == Some(Chunk(0)._2))

    // optimized seek
    reader.seek(0)
    assert(seekCount == 2)  // No seek!
    assert(reader.read() == Some(Chunk(0)._2))
    reader.seek(2)
    assert(seekCount == 2)  // No seek!
    assert(reader.read() == Some(Chunk(1)._2))
    reader.seek(2+3)
    assert(seekCount == 2)  // No seek!
    assert(reader.read() == Some(Chunk(2)._2))

  "Last truncated record of truncated file is ignored" - {
    "Last record is not truncated" in:
      assert(read("").isEmpty)

    "Truncated file" in:
      assert(read("{").isEmpty)

    "Truncated file before LF" in:
      var _seek = -1L
      val in = new ByteArrayInputStream(Array[Byte]('{')) with SeekableInputStream:
        def seek(pos: Long) = _seek = pos
      val reader = newInputStreamJsonSeqReader(in, blockSize = 1)
      assert(reader.read().isEmpty)
      assert(reader.position == 0)  // Position unchanged, before the truncated record
      assert(_seek == 0)
  }

  "Corrupt data" - {
    "Missing separators" in:
      assert(expectException("{}{}\n").toStringWithCauses ==
        "JSON sequence is corrupt at line 1: expected whitespace or eof got '{}\\n' (column 3)")

    "Invalid JSON" in:
      assert(expectException("{\n").toStringWithCauses ==
        "JSON sequence is corrupt at line 1: exhausted input")
  }

  "Reading closed" in:
    withTemporaryFile: file =>
      val in = SeekableInputStream.openFile(file)
      val reader = newInputStreamJsonSeqReader(in, blockSize = 1)
      reader.close()
      intercept[InputStreamJsonSeqReader.ClosedException]:
        reader.read()
      .toStringWithCauses shouldEqual s"JsonSeqFileClosed: JSON sequence from file 'JSON-SEQ-TEST' has been closed"

  "InputStreamJsonSeqReader" in:
    for blockSize <- 1 to 2 * FourByteUtf8.length do
      for n <- 0 to 3 do
        withClue(s"blockSize=$blockSize n=$n:\n"):
          val expected = for i <- 0 until n; (_, PositionAnd(pos, json)) <- Chunk yield PositionAnd(i * ChunkBytes.size + pos, json)
          val bytes = Array.fill(n)(ChunkBytes).flatten
          val posAndJsons = newInputStreamJsonSeqReader(simplifiedSeekableInputStream(bytes), blockSize = blockSize)
            .iterator.toVector
          assert(posAndJsons == expected)

private object InputStreamJsonSeqReaderTest:

  private val Chunk: Seq[(Array[Byte], PositionAnd[Json])] = Vector(
      "1"       -> PositionAnd(0    , Json.fromInt(1)),
      "23"      -> PositionAnd(2    , Json.fromInt(23)),
      "\"x\""   -> PositionAnd(2+3  , Json.fromString("x")),
      "\"x🥕\"" -> PositionAnd(2+3+4, Json.fromString("x🥕")))
    .map(o => o.copy(_1 = o._1.getBytes(UTF_8)))
  private val ChunkBytes: Seq[Byte] = Chunk.flatMap(o => o._1 :+ LF)

  private val FourByteUtf8 = Vector('"', 'x', 0xF0.toByte, 0x9F.toByte, 0xA5.toByte, 0x95.toByte, '"')
  assert(Chunk.last._1.toVector == FourByteUtf8)

  private def read(bytes: String) =
    newInputStreamJsonSeqReader(simplifiedSeekableInputStream(bytes.getBytes(US_ASCII))).read()

  private def newInputStreamJsonSeqReader(in: SeekableInputStream, blockSize: Int = InputStreamJsonSeqReader.BlockSize) =
    new InputStreamJsonSeqReader(in, name = "JSON-SEQ-TEST", blockSize = blockSize)

  private def expectException(bytes: String): Exception =
    intercept[Exception]:
      read(bytes)

  private def simplifiedSeekableInputStream(bytes: Array[Byte]): SeekableInputStream =
    new ByteArrayInputStream(bytes) with SeekableInputStream:
      def seek(pos: Long) = assert(pos == 0)  // Only seek(0) after truncated last record

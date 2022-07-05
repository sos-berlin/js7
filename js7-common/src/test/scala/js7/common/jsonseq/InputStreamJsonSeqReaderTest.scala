package js7.common.jsonseq

import io.circe.Json
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets.{US_ASCII, UTF_8}
import js7.base.io.file.FileUtils.withTemporaryFile
import js7.base.utils.Ascii.{LF, RS}
import js7.base.utils.ScalaUtils.syntax._
import js7.common.jsonseq.InputStreamJsonSeqReaderTest._
import js7.common.message.ProblemCodeMessages
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.language.reflectiveCalls

/**
  * @author Joacim Zschimmer
  */
final class InputStreamJsonSeqReaderTest extends AnyFreeSpec
{
  ProblemCodeMessages.initialize()

  "Empty file" in {
    val in = new InputStream with SeekableInputStream {
      def read() = -1
      def seek(pos: Long) = throw new NotImplementedError
    }
    val reader = newInputStreamJsonSeqReader(in)
    assert(reader.read() == None)
  }

  "read, seek" in {
    val in = new InputStream with SeekableInputStream {
      var pos = 0
      var seekCount = 0
      def read(): Int =
        if (pos == ChunkBytes.length)
          -1
        else {
          pos += 1
          ChunkBytes(pos - 1)
        }
      def seek(pos: Long) = {
        seekCount += 1
        this.pos = pos.toInt
      }
    }
    val reader = newInputStreamJsonSeqReader(in, blockSize = 4)
    assert(reader.read() == Some(Chunk(0)._2))
    assert(reader.read() == Some(Chunk(1)._2))
    assert(reader.read() == Some(Chunk(2)._2))
    assert(reader.read() == Some(Chunk(3)._2))
    assert(reader.read() == None)
    assert(in.seekCount == 0)
    assert(reader.read() == None)

    // seek
    reader.seek(3+4)
    assert(in.seekCount == 1)
    assert(reader.read() == Some(Chunk(2)._2))
    reader.seek(0)
    assert(in.seekCount == 2)
    assert(reader.read() == Some(Chunk(0)._2))

    // optimized seek
    reader.seek(0)
    assert(in.seekCount == 2)  // No seek!
    assert(reader.read() == Some(Chunk(0)._2))
    reader.seek(3)
    assert(in.seekCount == 2)  // No seek!
    assert(reader.read() == Some(Chunk(1)._2))
    reader.seek(3+4)
    assert(in.seekCount == 2)  // No seek!
    assert(reader.read() == Some(Chunk(2)._2))
  }

  "Last truncated record of truncated file is ignored" - {
    "Last record is not truncated" in {
      assert(read("").isEmpty)
    }

    "Truncated file" in {
      assert(read(s"$rs{", withRS = true).isEmpty)
      assert(read("{").isEmpty)
    }

    "Truncated file before LF" in {
      val in = new ByteArrayInputStream(Array[Byte](RS, '{')) with SeekableInputStream {
        var _seek = -1L
        def seek(pos: Long) = _seek = pos
      }
      val reader = newInputStreamJsonSeqReader(in, blockSize = 1)
      assert(reader.read().isEmpty)
      assert(reader.position == 0)  // Position unchanged, before the truncated record
      assert(in._seek == 0)
    }

    "Truncated file after RS" in {
      assert(read(s"$rs").isEmpty)
    }
  }

  "Corrupt data, with RS" - {
    "Missing RS" in {
      assert(expectException("{}\n", withRS = true).toStringWithCauses ==
        "JSON sequence is corrupt at line 1: Missing ASCII RS at start of JSON sequence record (instead read: 7b)")
    }

    "Missing separators" in {
      assert(expectException(s"$rs{}{}\n", withRS = true).toStringWithCauses ==
        "JSON sequence is corrupt at line 1: expected whitespace or eof got '{}\\n' (column 3)")
    }

    "Invalid JSON" in {
      assert(expectException(s"$rs{\n", withRS = true).toStringWithCauses ==
        "JSON sequence is corrupt at line 1: exhausted input")
    }
  }

  "Corrupt data, without RS" - {
    "Missing separators" in {
      assert(expectException("{}{}\n").toStringWithCauses ==
        "JSON sequence is corrupt at line 1: expected whitespace or eof got '{}\\n' (column 3)")
    }

    "Invalid JSON" in {
      assert(expectException("{\n").toStringWithCauses ==
        "JSON sequence is corrupt at line 1: exhausted input")
    }
  }

  "Reading closed" in {
    withTemporaryFile { file =>
      val in = SeekableInputStream.openFile(file)
      val reader = newInputStreamJsonSeqReader(in, blockSize = 1)
      reader.close()
      intercept[InputStreamJsonSeqReader.ClosedException] {
        reader.read()
      }.toStringWithCauses shouldEqual s"JsonSeqFileClosed: JSON sequence from file 'JSON-SEQ-TEST' has been closed"
    }
  }

  "InputStreamJsonSeqReader" in {
    for (blockSize <- 1 to 2 * FourByteUtf8.length) {
      for (n <- 0 to 3) {
        withClue(s"blockSize=$blockSize n=$n:\n") {
          val expected = for (i <- 0 until n; (_, PositionAnd(pos, json)) <- Chunk) yield PositionAnd(i * ChunkBytes.size + pos, json)
          val bytes = Array.fill(n)(ChunkBytes).flatten
          val posAndJsons = newInputStreamJsonSeqReader(simplifiedSeekableInputStream(bytes), blockSize = blockSize)
            .iterator.toVector
          assert(posAndJsons == expected)
        }
      }
    }
  }
}

private object InputStreamJsonSeqReaderTest
{
  private val rs = RS.toChar
  private val Chunk: Seq[(Array[Byte], PositionAnd[Json])] = Vector(
      "1"       -> PositionAnd(0    , Json.fromInt(1)),
      "23"      -> PositionAnd(3    , Json.fromInt(23)),
      "\"x\""   -> PositionAnd(3+4  , Json.fromString("x")),
      "\"x🥕\"" -> PositionAnd(3+4+5, Json.fromString("x🥕")))
    .map(o => o.copy(_1 = o._1.getBytes(UTF_8)))
  private val ChunkBytes: Seq[Byte] = Chunk.flatMap(o => Array(RS) ++ o._1 :+ LF)

  private val FourByteUtf8 = Vector('"', 'x', 0xF0.toByte, 0x9F.toByte, 0xA5.toByte, 0x95.toByte, '"')
  assert(Chunk.last._1 sameElements FourByteUtf8)

  private def read(bytes: String, withRS: Boolean = false) =
    newInputStreamJsonSeqReader(simplifiedSeekableInputStream(bytes.getBytes(US_ASCII)), withRS = withRS).read()

  private def newInputStreamJsonSeqReader(in: SeekableInputStream, blockSize: Int = InputStreamJsonSeqReader.BlockSize, withRS: Boolean = true) =
    new InputStreamJsonSeqReader(in, name = "JSON-SEQ-TEST", blockSize = blockSize, withRS = withRS)

  private def expectException(bytes: String, withRS: Boolean = false): Exception =
    intercept[Exception] {
      read(bytes, withRS = withRS)
    }

  private def simplifiedSeekableInputStream(bytes: Array[Byte]): SeekableInputStream =
    new ByteArrayInputStream(bytes) with SeekableInputStream {
      def seek(pos: Long) = assert(pos == 0)  // Only seek(0) after truncated last record
    }
}

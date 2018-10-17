package com.sos.jobscheduler.core.common.jsonseq

import com.google.common.base.Ascii.{LF, RS}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryFile
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReaderTest._
import io.circe.Json
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets.UTF_8
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.immutable.Seq
import scala.language.reflectiveCalls

/**
  * @author Joacim Zschimmer
  */
final class InputStreamJsonSeqReaderTest extends FreeSpec {

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
    val reader = new InputStreamJsonSeqReader(in, blockSize = 4)
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

  "Corrupt data" - {
    def expectException(bytes: Byte*): Exception =
      intercept[Exception] {
        new InputStreamJsonSeqReader(simplifiedSeekableInputStream(bytes.toArray)).read()
      }

    "Truncated JSON" in {
      assert(expectException(RS, '{').toStringWithCauses ==
        "JSON sequence is corrupt at line 1: Missing ASCII LF at end of JSON sequence record")
    }

    "Truncated before LF" in {
      assert(expectException(RS, '{', '}').toStringWithCauses ==
        "JSON sequence is corrupt at line 1: Missing ASCII LF at end of JSON sequence record")
    }

    "Truncated after RS" in {
      assert(expectException(RS).toStringWithCauses ==
        "JSON sequence is corrupt at line 1: Missing ASCII LF at end of JSON sequence record")
    }

    "Missing RS" in {
      assert(expectException('{', '}', LF).toStringWithCauses ==
        "JSON sequence is corrupt at line 1: Missing ASCII RS at start of JSON sequence record")
    }

    "Missing separators" in {
      assert(expectException(RS, '{', '}', '{', '}', LF).toStringWithCauses ==
        "JSON sequence is corrupt at line 1: expected whitespace or eof got { (column 3)")
    }

    "Invalid JSON" in {
      assert(expectException(RS, '{', LF).toStringWithCauses ==
        "JSON sequence is corrupt at line 1: exhausted input")
    }
  }

  "Reading closed" in {
    withTemporaryFile { file ⇒
      val in = SeekableInputStream.openFile(file)
      val reader = new InputStreamJsonSeqReader(in, blockSize = 1)
      reader.close()
      intercept[Exception] {
        reader.read()
      }.toStringWithCauses shouldEqual "JSON sequence file has been closed while reading"
    }
  }

  "InputStreamJsonSeqReader" in {
    for (blockSize ← 1 to 2 * FourByteUtf8.length) {
      for (n ← 0 to 3) {
        withClue(s"blockSize=$blockSize n=$n:\n") {
          val expected = for (i ← 0 until n; (_, PositionAnd(pos, json)) ← Chunk) yield PositionAnd(i * ChunkBytes.size + pos, json)
          val bytes = Array.fill(n)(ChunkBytes).flatten
          val posAndJsons = new InputStreamJsonSeqReader(simplifiedSeekableInputStream(bytes), blockSize = blockSize)
            .iterator.toVector
          assert(posAndJsons == expected)
        }
      }
    }
  }
}

private object InputStreamJsonSeqReaderTest {
  private val Chunk: Seq[(Array[Byte], PositionAnd[Json])] = Vector(
      "1"       → PositionAnd(0    , Json.fromInt(1)),
      "23"      → PositionAnd(3    , Json.fromInt(23)),
      "\"x\""   → PositionAnd(3+4  , Json.fromString("x")),
      "\"x🥕\"" → PositionAnd(3+4+5, Json.fromString("x🥕")))
    .map(o ⇒ o.copy(_1 = o._1.getBytes(UTF_8)))
  private val ChunkBytes: Seq[Byte] = Chunk flatMap (o ⇒ Array(RS) ++ o._1 :+ LF)

  private val FourByteUtf8 = Vector('"', 'x', 0xF0.toByte, 0x9F.toByte, 0xA5.toByte, 0x95.toByte, '"')
  assert(Chunk.last._1 sameElements FourByteUtf8)

  private def simplifiedSeekableInputStream(bytes: Array[Byte]): SeekableInputStream =
    new ByteArrayInputStream(bytes) with SeekableInputStream {
      def seek(pos: Long) = throw new NotImplementedError
    }
}

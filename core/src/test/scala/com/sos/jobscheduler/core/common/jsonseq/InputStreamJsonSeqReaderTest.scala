package com.sos.jobscheduler.core.common.jsonseq

import com.google.common.base.Ascii.{LF, RS}
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReaderTest._
import io.circe.Json
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets.UTF_8
import org.scalatest.FreeSpec
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
      def read(): Int = {
        if (pos == ChunkBytes.length)
          -1
        else {
          pos += 1
          ChunkBytes(pos - 1)
        }
      }
      def seek(pos: Long) = {
        seekCount += 1
        this.pos = pos.toInt
      }
    }
    val reader = new InputStreamJsonSeqReader(in, blockSize = 4, charBufferSize = 4)
    assert(reader.read() == Some(Chunk(0)._2))
    assert(reader.read() == Some(Chunk(1)._2))
    assert(reader.read() == Some(Chunk(2)._2))
    assert(reader.read() == Some(Chunk(3)._2))
    assert(reader.read() == None)
    assert(in.seekCount == 0)

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

  "InputStreamJsonSeqReader" in {
    for (blockSize ← 1 to 2 * FourByteUtf8.length; charBufferSize ← 1 to FourByteUtf8.length) {
      for (n ← 0 to 3) {
        withClue(s"blockSize=$blockSize charBufferSize=$charBufferSize n=$n:\n") {
          val expected = for (i ← 0 until n; (_, PositionAnd(pos, json)) ← Chunk) yield PositionAnd(i * ChunkBytes.size + pos, json)
          val bytes = Array.fill(n)(ChunkBytes).flatten
          val in = new ByteArrayInputStream(bytes) with SeekableInputStream {
            def seek(pos: Long) = throw new NotImplementedError
          }
          val posAndJsons = new InputStreamJsonSeqReader(in, blockSize = blockSize, charBufferSize = charBufferSize).iterator.toVector
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
}

package com.sos.jobscheduler.core.common.jsonseq

import com.google.common.base.Ascii.{LF, RS}
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReaderTest._
import io.circe.Json
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets.UTF_8
import org.scalatest.FreeSpec
import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
final class InputStreamJsonSeqReaderTest extends FreeSpec {

  "InputStreamJsonSeqReader" in {
    for (blockSize ← 1 to 2 * FourByteUtf8.length; charBufferSize ← 1 to FourByteUtf8.length) {
      for (n ← 0 to 3) {
        withClue(s"blockSize=$blockSize charBufferSize=$charBufferSize n=$n:\n") {
          val expected = for (i ← 0 until n; (_, PositionAnd(pos, json)) ← Chunk) yield PositionAnd(i * ChunkSize + pos, json)
          val bytes = Array.fill(n)(Chunk flatMap (o ⇒ Array(RS) ++ o._1 :+ LF)).flatten
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
  private val Chunk: immutable.Seq[(Array[Byte], PositionAnd[Json])] = Vector(
      "1"       → PositionAnd(0    , Json.fromInt(1)),
      "23"      → PositionAnd(3    , Json.fromInt(23)),
      "\"x\""   → PositionAnd(3+4  , Json.fromString("x")),
      "\"x🥕\"" → PositionAnd(3+4+5, Json.fromString("x🥕")))
    .map(o ⇒ o.copy(_1 = o._1.getBytes(UTF_8)))
  private val ChunkSize = Chunk.map(_._1.length + 2/*RS,LF*/).sum

  private val FourByteUtf8 = Vector('"', 'x', 0xF0.toByte, 0x9F.toByte, 0xA5.toByte, 0x95.toByte, '"')
  assert(Chunk.last._1 sameElements FourByteUtf8)
}

package js7.base.io

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.{Deflater, Inflater, InflaterInputStream}
import js7.base.test.OurTestSuite
import js7.base.utils.Atomic.extensions.+=
import org.scalatest.matchers.should.Matchers.*
import scala.collection.mutable
import scala.io.Source

final class ChunkedDeflaterOutputStreamTest extends OurTestSuite:

  "finishChunk creates independent compressed chunks" in:
    val baos = new ByteArrayOutputStream()
    val deflater = new Deflater()
    val out = new ChunkedDeflaterOutputStream(baos, deflater)

    val chunk1 = "First chunk of data"
    val chunk2 = "Second chunk of data"
    val chunk3 = "Third chunk"

    out.write(chunk1.getBytes("UTF-8"))
    val pos1 = out.finishChunk()

    out.write(chunk2.getBytes("UTF-8"))
    val pos2 = out.finishChunk()

    out.write(chunk3.getBytes("UTF-8"))
    out.close()

    val compressed = baos.toByteArray
    val positions = Seq(pos1, pos2)

    // Inflate each chunk independently
    val result1 = inflateSingleChunk(compressed, 0)
    result1 shouldBe chunk1

    val result2 = inflateSingleChunk(compressed, pos1.toInt)
    result2 shouldBe chunk2

    val result3 = inflateSingleChunk(compressed, pos2.toInt)
    result3 shouldBe chunk3

    // Inflate all chunks as a continuous stream (needs manual inflater reset)
    val wholeResult = inflateAllChunks(compressed, Seq(0L) ++ positions)
    wholeResult shouldBe (chunk1 + chunk2 + chunk3)

  "whole stream is inflatable as a continuous stream" in:
    val baos = new ByteArrayOutputStream()
    val deflater = new Deflater()
    val out = new ChunkedDeflaterOutputStream(baos, deflater)
    val chunks = (1 to 5).map(i => s"Chunk $i content\n").toSeq
    val positions = mutable.Buffer[Long]()

    positions += 0

    // Write in chunks
    chunks.foreach: chunk =>
      out.write(chunk.getBytes("UTF-8"))
      positions += out.finishChunk()

    out.close()

    val compressed = baos.toByteArray

    // Inflate entire stream by processing each chunk
    val result = inflateAllChunks(compressed, positions.toSeq)
    result shouldBe chunks.mkString

  "handles empty chunks" in:
    val baos = new ByteArrayOutputStream()
    val deflater = new Deflater()
    val out = new ChunkedDeflaterOutputStream(baos, deflater)
    val positions = mutable.Buffer[Long]()
    positions += 0

    out.write("data".getBytes("UTF-8"))
    positions += out.finishChunk()
    positions += out.finishChunk() // Empty chunk
    out.write("more".getBytes("UTF-8"))
    out.close()

    val compressed = baos.toByteArray
    val result = inflateAllChunks(compressed, positions.toSeq)
    result shouldBe "datamore"

  "single byte writes work" in:
    val baos = new ByteArrayOutputStream()
    val deflater = new Deflater()
    val out = new ChunkedDeflaterOutputStream(baos, deflater)

    "ABC".foreach(c => out.write(c.toInt))
    out.close()

    val result = inflateSingleChunk(baos.toByteArray, 0)
    result shouldBe "ABC"

  "ChunkedInflaterInputStream reads all chunks from position" in:
    val baos = new ByteArrayOutputStream()
    val deflater = new Deflater()
    val out = new ChunkedDeflaterOutputStream(baos, deflater)

    val chunk1 = "First chunk"
    val chunk2 = "Second chunk"
    val chunk3 = "Third chunk"

    out.write(chunk1.getBytes("UTF-8"))
    val pos1 = out.finishChunk()

    out.write(chunk2.getBytes("UTF-8"))
    val pos2 = out.finishChunk()

    out.write(chunk3.getBytes("UTF-8"))
    out.close()

    val compressed = baos.toByteArray

    // Read from beginning - should get all chunks
    val resultFromStart = inflateWithChunkedStream(compressed, 0)
    resultFromStart shouldBe (chunk1 + chunk2 + chunk3)

    // Read from second chunk - should get chunk2 + chunk3
    val resultFromPos1 = inflateWithChunkedStream(compressed, pos1.toInt)
    resultFromPos1 shouldBe (chunk2 + chunk3)

    // Read from third chunk - should get only chunk3
    val resultFromPos2 = inflateWithChunkedStream(compressed, pos2.toInt)
    resultFromPos2 shouldBe chunk3

  "ChunkedInflaterInputStream handles many chunks" in:
    val baos = new ByteArrayOutputStream()
    val deflater = new Deflater()
    val out = new ChunkedDeflaterOutputStream(baos, deflater)

    val chunks = (1 to 10).map(i => s"Chunk number $i\n").toSeq

    chunks.foreach: chunk =>
      out.write(chunk.getBytes("UTF-8"))
      out.finishChunk()

    out.close()

    val result = inflateWithChunkedStream(baos.toByteArray, 0)
    result shouldBe chunks.mkString

  /** Inflates a single chunk starting at the given offset */
  private def inflateSingleChunk(compressed: Array[Byte], offset: Int): String =
    val bais = new ByteArrayInputStream(compressed, offset, compressed.length - offset)
    val in = new InflaterInputStream(bais)
    try
      Source.fromInputStream(in, "UTF-8").mkString
    finally
      in.close()

  /** Inflates all chunks by resetting the inflater between chunks */
  private def inflateAllChunks(compressed: Array[Byte], positions: Seq[Long]): String =
    val result = new StringBuilder
    val inflater = new Inflater()

    for i <- positions.indices do
      val start = positions(i).toInt
      val end = if i + 1 < positions.length then positions(i + 1).toInt else compressed.length

      inflater.reset()
      inflater.setInput(compressed, start, end - start)

      val buffer = new Array[Byte](1024)
      while !inflater.finished() do
        val count = inflater.inflate(buffer)
        if count > 0 then
          result.append(new String(buffer, 0, count, "UTF-8"))

    inflater.end()
    result.toString

  /** Inflates using ChunkedInflaterInputStream from the given offset */
  private def inflateWithChunkedStream(compressed: Array[Byte], offset: Int): String =
    val bais = new ByteArrayInputStream(compressed, offset, compressed.length - offset)
    val inflater = new Inflater()
    val in = new ChunkedInflaterInputStream(bais, inflater)
    try
      Source.fromInputStream(in, "UTF-8").mkString
    finally
      in.close()
      inflater.end()

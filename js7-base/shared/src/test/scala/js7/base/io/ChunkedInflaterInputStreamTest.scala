package js7.base.io

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.{Deflater, Inflater}
import js7.base.test.OurTestSuite
import org.scalatest.matchers.should.Matchers.*
import scala.io.Source

final class ChunkedInflaterInputStreamTest extends OurTestSuite:

  "reads all chunks from beginning" in:
    val (compressed, _) = createCompressedChunks(Seq("First", "Second", "Third"))

    val result = inflateFrom(compressed, 0)
    result shouldBe "FirstSecondThird"

  "reads from middle position" in:
    val (compressed, positions) = createCompressedChunks(Seq("First", "Second", "Third"))

    val result = inflateFrom(compressed, positions(1).toInt)
    result shouldBe "SecondThird"

  "reads from last chunk position" in:
    val (compressed, positions) = createCompressedChunks(Seq("First", "Second", "Third"))

    val result = inflateFrom(compressed, positions(2).toInt)
    result shouldBe "Third"

  "handles many small chunks" in:
    val chunks = (1 to 20).map(i => s"$i ").toSeq
    val (compressed, _) = createCompressedChunks(chunks)

    val result = inflateFrom(compressed, 0)
    result shouldBe chunks.mkString

  "handles large chunks" in:
    val chunk1 = "A" * 10000
    val chunk2 = "B" * 10000
    val chunk3 = "C" * 10000
    val (compressed, positions) = createCompressedChunks(Seq(chunk1, chunk2, chunk3))

    val result = inflateFrom(compressed, positions(1).toInt)
    result shouldBe (chunk2 + chunk3)

  "handles empty stream" in:
    val baos = new ByteArrayOutputStream()
    val deflater = new Deflater()
    val out = new ChunkedDeflaterOutputStream(baos, deflater)
    out.close()

    val result = inflateFrom(baos.toByteArray, 0)
    result shouldBe ""

  "handles single chunk" in:
    val (compressed, _) = createCompressedChunks(Seq("OnlyChunk"))

    val result = inflateFrom(compressed, 0)
    result shouldBe "OnlyChunk"

  "handles byte-by-byte reading" in:
    val (compressed, _) = createCompressedChunks(Seq("AB", "CD", "EF"))

    val bais = new ByteArrayInputStream(compressed)
    val inflater = new Inflater()
    val in = new ChunkedInflaterInputStream(bais, inflater)

    try
      val result = new StringBuilder
      var b = in.read()
      while b != -1 do
        result.append(b.toChar)
        b = in.read()
      result.toString shouldBe "ABCDEF"
    finally
      in.close()
      inflater.end()

  "available returns reasonable values" in:
    val (compressed, _) = createCompressedChunks(Seq("chunk1", "chunk2"))

    val bais = new ByteArrayInputStream(compressed)
    val inflater = new Inflater()
    val in = new ChunkedInflaterInputStream(bais, inflater)

    try
      in.available() should be >= 0
      in.read() // Read one byte
      in.available() should be >= 0
      // Read all remaining
      while in.read() != -1 do ()
      in.available() shouldBe 0
    finally
      in.close()
      inflater.end()

  private def createCompressedChunks(chunks: Seq[String]): (Array[Byte], Seq[Long]) =
    val baos = new ByteArrayOutputStream()
    val deflater = new Deflater()
    val out = new ChunkedDeflaterOutputStream(baos, deflater)

    val positions = Seq.newBuilder[Long]
    positions += 0L // First chunk starts at 0

    chunks.foreach: chunk =>
      out.write(chunk.getBytes("UTF-8"))
      val pos = out.finishChunk()
      positions += pos

    out.close()
    val allPositions = positions.result()
    // Return positions without the last one (which is the end position after all chunks)
    (baos.toByteArray, allPositions.init)

  private def inflateFrom(compressed: Array[Byte], offset: Int): String =
    val bais = new ByteArrayInputStream(compressed, offset, compressed.length - offset)
    val inflater = new Inflater()
    val in = new ChunkedInflaterInputStream(bais, inflater)
    try
      Source.fromInputStream(in, "UTF-8").mkString
    finally
      in.close()
      inflater.end()

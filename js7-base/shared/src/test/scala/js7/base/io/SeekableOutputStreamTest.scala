package js7.base.io

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.io.OpaquePos
import js7.base.test.OurTestSuite
import js7.base.utils.AutoClosing.autoClosing
import org.scalatest.matchers.should.Matchers.*
import scala.collection.mutable

// Test SeekableInputStream and SeekableOutputStream
trait SeekableOutputStreamTest extends OurTestSuite:

  protected def newChunkedOutputStream(out: OutputStream): SeekableOutputStream
  protected def newChunkedInputStream(in: InputStream): SeekableInputStream

  "markOpaquePos creates independently compressed chunks" in:
    val baos = new ByteArrayOutputStream()
    val out = newChunkedOutputStream(baos)

    val chunk1 = "First chunk of data"
    val chunk2 = "Second chunk of data"
    val chunk3 = "Third chunk"

    out.write(chunk1.getBytes(UTF_8))
    val pos1 = out.markOpaquePos()

    out.write(chunk2.getBytes(UTF_8))
    val pos2 = out.markOpaquePos()

    out.write(chunk3.getBytes(UTF_8))
    out.close()

    val compressed = baos.toByteArray
    val positions = Seq(pos1, pos2)

    // Inflate each chunk independently
    val result1 = decompressFromPosition(compressed, 0)
    result1 shouldBe chunk1 + chunk2 + chunk3

    val result2 = decompressFromPosition(compressed, pos1.toLong.toInt)
    result2 shouldBe chunk2 + chunk3

    val result3 = decompressFromPosition(compressed, pos2.toLong.toInt)
    result3 shouldBe chunk3

  "Empty chunks" in:
    val baos = new ByteArrayOutputStream()
    val out = newChunkedOutputStream(baos)
    val positions = mutable.Buffer[OpaquePos]()
    positions += OpaquePos(0)

    out.write("data".getBytes(UTF_8))
    positions += out.markOpaquePos()
    positions += out.markOpaquePos() // Empty chunk
    out.write("more".getBytes(UTF_8))
    out.close()

    val compressed = baos.toByteArray
    val result = decompressFromPosition(compressed, 0)
    result shouldBe "datamore"

  "Single byte write" in:
    val baos = new ByteArrayOutputStream()
    val out = newChunkedOutputStream(baos)

    "ABC".foreach(c => out.write(c.toInt))
    out.close()

    val result = decompressFromPosition(baos.toByteArray, 0)
    result shouldBe "ABC"

  "Many small chunks" in :
    val baos = new ByteArrayOutputStream()
    val out = newChunkedOutputStream(baos)

    val chunks = (1 to 1000).map(i => s"$i ")
    chunks.foreach: chunk =>
      out.write(chunk.getBytes(UTF_8))
      out.markOpaquePos()

    val result = decompressFromPosition(baos.toByteArray, 0)
    result shouldBe chunks.mkString

  "Large chunks" in :
    val baos = new ByteArrayOutputStream()
    val out = newChunkedOutputStream(baos)

    val chunks = (1 to 10).map(i => s"$i " + "+" * 100_000 + "\n")
    chunks.foreach: chunk =>
      out.write(chunk.getBytes(UTF_8))
      out.markOpaquePos()

    val result = decompressFromPosition(baos.toByteArray, 0)
    result shouldBe chunks.mkString

  private def decompressFromPosition(compressed: Array[Byte], offset: Int): String =
    val bais = new ByteArrayInputStream(compressed, offset, compressed.length - offset)
    autoClosing(newChunkedInputStream(bais)): in =>
      new String(in.readAllBytes(), UTF_8)

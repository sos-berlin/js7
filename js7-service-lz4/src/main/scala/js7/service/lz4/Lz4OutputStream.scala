package js7.service.lz4

import java.io.{IOException, OutputStream}
import js7.base.io.{OpaquePos, SeekableOutputStream}
import js7.service.lz4.Lz4OutputStream.*
import net.jpountz.lz4.LZ4Compressor

/** An LZ4-compressing output stream that supports independent compressed chunks.
  *
  * Each chunk is compressed independently, making them separately decompressable.
  * The chunks are concatenated in the output stream.
  *
  * Call `markOpaquePos()` to complete the current independent chunk and get its
  * end position in the output stream. Use these positions to decompress individual chunks.
  *
  * Chunk format: [4 bytes: decompressed length][4 bytes: compressed length][compressed data]
  *
  * @param out The underlying output stream
  */
final class Lz4OutputStream(
  out: OutputStream,
  makeCompressor: () => LZ4Compressor,
  bufferSize: Int)
extends SeekableOutputStream(out):

  private var byteCount = 0L
  private var compressor = makeCompressor()
  private val compressedBuffer = new Array[Byte](8 + compressor.maxCompressedLength(bufferSize))
  private val inputMax = bufferSize
  private val singleByte = new Array[Byte](1)
  private var closed = false

  assert(compressedBuffer.length == toCompressedBufferSize(bufferSize))

  override def write(b: Int): Unit =
    ensureOpen()
    singleByte(0) = b.toByte
    write(singleByte, 0, 1)

  override def write(array: Array[Byte]): Unit =
    write(array, 0, array.length)

  override def write(array: Array[Byte], offset: Int, length: Int): Unit =
    ensureOpen()
    val end = offset + length
    var i = offset
    while i < end do
      val len = length - i min inputMax
      val compressedLen =
        compressor.compress(
          array, i, len,
          compressedBuffer, 8, compressedBuffer.length - 8)

      writeInt(compressedBuffer, 0, len)
      writeInt(compressedBuffer, 4, compressedLen)

      out.write(compressedBuffer, 0, 8 + compressedLen)
      byteCount += 8 + compressedLen
      i += len

  def markOpaquePos(): OpaquePos =
    ensureOpen()
    compressor = makeCompressor()
    OpaquePos(byteCount)

  override def flush(): Unit =
    ensureOpen()
    out.flush()

  override def close(): Unit =
    if !closed then
      out.close()
      closed = true

  private def ensureOpen(): Unit =
    if closed then throw new IOException("OutputStream closed")


object Lz4OutputStream:

  private val MaxDecompressedChunkSize = 0x7E000000 - 1

  private def writeInt(array: Array[Byte], offset: Int, int: Int) =
    array(offset + 0) = (int >>> 24).toByte
    array(offset + 1) = (int >>> 16).toByte
    array(offset + 2) = (int >>> 8).toByte
    array(offset + 3) = int.toByte

  // Copied from package-private LZ4Utils
  private[lz4] def toCompressedBufferSize(decompressedBufferSize: Int) =
    if decompressedBufferSize < 0 then throw new IllegalArgumentException(
      s"decompressedBufferSize must be >= 0, got $decompressedBufferSize")
    if decompressedBufferSize > MaxDecompressedChunkSize then throw new IllegalArgumentException(
      s"decompressedBufferSize must be <= $MaxDecompressedChunkSize")
    8/*length bytes*/ + decompressedBufferSize + decompressedBufferSize / 255 + 16

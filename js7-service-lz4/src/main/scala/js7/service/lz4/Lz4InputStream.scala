package js7.service.lz4

import java.io.{IOException, InputStream}
import js7.base.io.SeekableInputStream
import js7.service.lz4.Lz4InputStream.*
import net.jpountz.lz4.LZ4FastDecompressor

/** An LZ4-decompressing input stream that reads chunked LZ4 data.
  *
  * Reads from a specific position and decompresses all chunks until EOF.
  * Each chunk is independently decompressable, as produced by Lz4ChunkedOutputStream.
  *
  * Chunk format: [4 bytes: decompressed length][4 bytes: compressed length][compressed data]
  *
  * @param in The underlying input stream positioned at the start of a chunk
  * @param decompressor The decompressor to use
  */
final class Lz4InputStream(
  in: InputStream,
  makeDecompressor: () => LZ4FastDecompressor,
  bufferSize: Int)
extends SeekableInputStream(in):

  private val compressedBuffer = new Array[Byte](Lz4OutputStream.toCompressedBufferSize(bufferSize))
  private var decompressedBuffer: Array[Byte] | Null = null
  private var decompressedBufferOffset = 0
  private var decompressedBufferLen = 0
  private val decompressor = makeDecompressor()
  private var eof = false
  private var closed = false
  private val singleByte = new Array[Byte](1)

  override def read(): Int =
    if read(singleByte, 0, 1) == -1 then
      -1
    else
      singleByte(0) & 0xFF

  override def read(array: Array[Byte], offset: Int, arrayLen: Int): Int =
    ensureOpen()
    var off = offset
    val end = offset + arrayLen
    while off < end do
      var ret = readNext(array, off, end - off)
      if ret == -1 then
        val len = off - offset
        return if len > 0 then len else -1
      off += ret
    off - offset

  private def readNext(array: Array[Byte], offset: Int, arrayLen: Int): Int =
    ensureOpen()
    if eof then
      -1
    else
      val remaining = decompressedBufferLen - decompressedBufferOffset
      if remaining > 0 then
        val a = remaining min arrayLen
        System.arraycopy(decompressedBuffer, decompressedBufferOffset, array, offset, a)
        decompressedBufferOffset += a
        a
      else
        decompressNextChunk(array, offset, arrayLen)

  private def decompressNextChunk(array: Array[Byte], offset: Int, arrayLen: Int): Int =
    var ret = in.read(compressedBuffer, 0, 8)
    if ret == -1 then
      eof = true
      -1
    else
      if ret != 8 then throw new IOException("LZ4 input is truncated")
      val decompressedLen = readInt(compressedBuffer, 0)
      val compressedLen = readInt(compressedBuffer, 4)
      if compressedLen > bufferSize then
        throw new IOException(s"LZ4 compressed length $compressedLen exceeds buffer size $bufferSize")

      ret = in.read(compressedBuffer, 0, compressedLen)
      if ret != compressedLen then throw new IOException("LZ4 input is truncated")

      if decompressedLen <= arrayLen then
        ret = decompressor.decompress(compressedBuffer, 0, array, offset, decompressedLen)
        if ret != compressedLen then
          throw new IOException("LZ4 decompressed length not as expected")
        decompressedLen
      else
        if decompressedBuffer == null then
          decompressedBuffer = new Array[Byte](bufferSize)
        ret = decompressor.decompress(
          compressedBuffer, 0, decompressedBuffer, 0, decompressedLen)
        if ret != compressedLen then
          throw new IOException("LZ4 decompressed length not as expected")
        val len = decompressedLen min arrayLen
        System.arraycopy(decompressedBuffer, 0, array, offset, len)
        decompressedBufferOffset = len
        decompressedBufferLen = decompressedLen
        len

  override def close(): Unit =
    if !closed then
      in.close()
      closed = true

  private def ensureOpen(): Unit =
    if closed then throw new IOException("InputStream closed")


object Lz4InputStream:

  private[lz4] val DefaultBufferSize = 8192

  private def readInt(array: Array[Byte], offset: Int): Int =
    (array(offset) << 24) |
      ((array(offset + 1) & 0xff) << 16) |
      ((array(offset + 2) & 0xff) << 8) |
      (array(offset + 3) & 0xff)

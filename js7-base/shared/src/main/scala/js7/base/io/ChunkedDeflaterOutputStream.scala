package js7.base.io

import java.io.{FilterOutputStream, OutputStream}
import java.util.zip.Deflater

/** A deflating output stream that supports independent compressed chunks.
  *
  * Each chunk is compressed independently with finish()+reset(), making them
  * separately inflatable. The chunks are concatenated in the output stream.
  *
  * Call `finishChunk()` to complete the current independent chunk and get its
  * position in the output stream. Use these positions to inflate individual chunks.
  *
  * The entire stream can also be inflated by concatenating all chunks with a
  * multi-stream capable inflater (like when using `Inflater` with reset between chunks).
  *
  * @param out The underlying output stream
  */
final class ChunkedDeflaterOutputStream(
  out: OutputStream,
  deflater: Deflater = new Deflater,
  bufferSize: Int = 512)
extends FilterOutputStream(out):

  private val buffer = new Array[Byte](bufferSize)
  private var byteCount = 0L
  private var closed = false

  override def write(b: Int): Unit =
    ensureOpen()
    val single = new Array[Byte](1)
    single(0) = b.toByte
    write(single, 0, 1)

  override def write(array: Array[Byte]): Unit =
    ensureOpen()
    write(array, 0, array.length)

  override def write(array: Array[Byte], offset: Int, length: Int): Unit =
    ensureOpen()
    if offset < 0 || length < 0 || offset + length > array.length then
      throw new IndexOutOfBoundsException()
    if length > 0 then
      deflater.setInput(array, offset, length)
      while !deflater.needsInput() do
        deflate()

  /** Finishes the current independent chunk.
    *
    * Completes the current deflate stream and resets the deflater for the next chunk.
    * Each chunk is independently inflatable starting from its returned position.
    *
    * @return The byte position where this chunk ends (and next chunk begins)
    */
  def finishChunk(): Long =
    ensureOpen()
    deflater.finish()
    while !deflater.finished() do
      deflate()
    deflater.reset()
    byteCount

  override def flush(): Unit =
    ensureOpen()
    out.flush()

  override def close(): Unit =
    if !closed then
      try
        finish()
      finally
        out.close()
        closed = true

  private def finish(): Unit =
    if !deflater.finished() then
      deflater.finish()
      while !deflater.finished() do
        deflate()

  private def deflate(): Unit =
    val len = deflater.deflate(buffer, 0, buffer.length)
    if len > 0 then
      out.write(buffer, 0, len)
      byteCount += len

  private def ensureOpen(): Unit =
    if closed then
      throw new java.io.IOException("Stream closed")

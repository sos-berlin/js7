package js7.base.io

import java.io.{FilterInputStream, IOException, InputStream}
import java.util.zip.Inflater
import scala.annotation.tailrec

/** An inflating input stream that reads chunked deflate data.
  *
  * Reads from a specific position and inflates all chunks until EOF.
  * Automatically resets the inflater between chunks to handle multiple
  * independent deflate streams concatenated together (as produced by
  * ChunkedDeflaterOutputStream).
  *
  * @param in The underlying input stream positioned at the start of a chunk
  * @param inflater The inflater to use (caller manages lifecycle)
  * @param bufferSize Size of the internal buffer for reading compressed data
  */
final class ChunkedInflaterInputStream(
  in: InputStream,
  inflater: Inflater = new Inflater,
  dictionary: Array[Byte] = Array.empty,
  bufferSize: Int = 512)
extends FilterInputStream(in):

  private val inputBuffer = new Array[Byte](bufferSize)
  private var bufferOffset = 0
  private var bufferLen = 0
  private var eof = false
  private var closed = false

  override def read(): Int =
    val single = new Array[Byte](1)
    if read(single, 0, 1) == -1 then -1
    else single(0) & 0xFF

  override def read(b: Array[Byte], off: Int, len: Int): Int =
    ensureOpen()
    if b == null then
      throw new NullPointerException()
    if off < 0 || len < 0 || len > b.length - off then
      throw new IndexOutOfBoundsException()
    if len == 0 then
      return 0
    if eof then
      return -1

    try
      @tailrec def readLoop(): Int =
        if inflater.finished() then
          // Current chunk finished
          // Calculate how many bytes were consumed from our buffer
          val consumed = bufferLen - inflater.getRemaining
          bufferOffset += consumed
          bufferLen = inflater.getRemaining

          // Reset inflater for next chunk
          inflater.reset()

          // If we have remaining bytes, feed them to the new chunk
          if bufferLen > 0 then
            inflater.setInput(inputBuffer, bufferOffset, bufferLen)

          readLoop()
        else if inflater.needsDictionary then
          inflater.setDictionary(dictionary)
          readLoop()
        else if inflater.needsInput then
          // Need more compressed data
          val n = in.read(inputBuffer)
          if n == -1 then
            // No more input from underlying stream
            eof = true
            -1
          else
            bufferOffset = 0
            bufferLen = n
            inflater.setInput(inputBuffer, 0, n)
            readLoop()
        else
          // Inflate available data
          val count = inflater.inflate(b, off, len)
          if count == 0 then
            readLoop() // Need more input or finished
          else
            count

      readLoop()
    catch
      case e: java.util.zip.DataFormatException =>
        throw new IOException("Invalid deflate data", e)

  override def available(): Int =
    ensureOpen()
    if eof then 0
    else if inflater.finished() then 0
    else 1

  override def close(): Unit =
    if !closed then
      in.close()
      closed = true

  private def ensureOpen(): Unit =
    if closed then
      throw new IOException("Stream closed")

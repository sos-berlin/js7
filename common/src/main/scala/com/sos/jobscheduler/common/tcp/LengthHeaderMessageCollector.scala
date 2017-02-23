package com.sos.scheduler.engine.common.tcp

import akka.util.{ByteString, ByteStringBuilder}
import java.nio.ByteBuffer
import java.nio.ByteOrder._
import org.jetbrains.annotations.TestOnly

/**
 * Converts a sequence of bytes, which starts with 4 length bytes (big endian) followed by the content,
 * to a ByteString containing the content only.
 *
 * @author Joacim Zschimmer
 */
final class LengthHeaderMessageCollector {
  private val lengthBuffer = ByteBuffer.allocate(4)
  private var length: Int = -1
  private var contentBuffer: ByteStringBuilder = null


  /**
   * @param chunk Next chunk of bytes, first chunk starts with 4 length bytes.
   * @return Some(content) if content according to length bytes is complete, None otherwise.
   */
  def apply(chunk: ByteString): Option[ByteString] = {
    if (lengthBuffer.hasRemaining) {
      val contentPosition = lengthBuffer.remaining
      val (nextLengthBytes, content) = chunk.splitAt(contentPosition)
      for (byte ← nextLengthBytes) lengthBuffer.put(byte)
      if (!lengthBuffer.hasRemaining) {
        length = lengthBuffer.getInt(0)
        if (length < 0) throw new IllegalArgumentException(f"Invalid (negative) length bytes: 0x$length%08x")
        contentBuffer = new ByteStringBuilder
        contentBuffer.append(content)
      }
    } else {
      contentBuffer.append(chunk)
    }
    if (contentBuffer == null || contentBuffer.length < length)
      None
    else{
      if (contentBuffer.length > length) throw new IllegalArgumentException(s"More data than expected: ${contentBuffer.length - length} extra bytes")  // No pipelining
      val result = contentBuffer.result()
      reset()
      Some(result)
    }
  }

  private def reset(): Unit = {
    lengthBuffer.rewind()
    length = -1
    contentBuffer = null
  }

  @TestOnly
  private[tcp] def isReset = lengthBuffer.position == 0 && lengthBuffer.limit == 4 && length == -1 && contentBuffer == null

  override def toString = getClass.getSimpleName concat "(" concat (
      if (isReset) "reset"
      else if (contentBuffer == null) s"expecting ${lengthBuffer.remaining()} length bytes"
      else s"received ${contentBuffer.length} of expected $length content bytes"
    ) concat ")"

  def expectedLength: Option[Int] = if (length == -1) None else Some(length)
}

object LengthHeaderMessageCollector {
  private implicit def byteOrder = BIG_ENDIAN

  /**
   * @return The Int argument as 4 bytes, big endian.
   */
  def intToBytesString(i: Int): ByteString = ByteString.newBuilder.putInt(i).result()
}

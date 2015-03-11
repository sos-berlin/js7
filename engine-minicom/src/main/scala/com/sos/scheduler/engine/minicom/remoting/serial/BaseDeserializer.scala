package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.remoting.serial.BaseDeserializer._
import java.nio.ByteBuffer
import java.util.UUID

/**
 * @author Joacim Zschimmer
 */
private[serial] trait BaseDeserializer {

  protected val buffer: ByteBuffer

  def hasData = buffer.remaining > 0

  def readInt16(): Short = buffer.getShort

  def readInt32(): Int = buffer.getInt

  def readInt64(): Long = buffer.getLong

  def readByte(): Byte = buffer.get

  def readBoolean(): Boolean = buffer.get != 0

  def readString(): String = {
    val length = buffer.getInt
    val b = new StringBuffer(length)
    for (i ← 1 to length) b.append(iso88591ByteToChar(buffer.get))
    b.toString
  }

  def readUUID(): UUID = {
    val high = buffer.getLong
    val low = buffer.getLong
    new UUID(high, low)
  }
}

private object BaseDeserializer {
  private def iso88591ByteToChar(o: Byte) = (o.toInt & 0xFF).toChar  // ISO-8859-1
}

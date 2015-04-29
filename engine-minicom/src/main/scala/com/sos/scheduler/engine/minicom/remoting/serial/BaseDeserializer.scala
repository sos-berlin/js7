package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.remoting.serial.BaseDeserializer._
import java.nio.ByteBuffer
import java.util.UUID

/**
 * @author Joacim Zschimmer
 */
private[serial] trait BaseDeserializer {

  protected val buffer: ByteBuffer

  final def hasData = buffer.remaining > 0

  final def readInt16(): Short = buffer.getShort

  final def readInt32(): Int = buffer.getInt

  final def readInt64(): Long = buffer.getLong

  final def readDouble(): Double = {
    require(readByte() == 's')
    val length = readByte()
    require(length > 0)
    val string = (for (_ ← 1 to length) yield readByte().toChar) mkString ""
    string.toDouble
  }

  final def readByte(): Byte = buffer.get

  final def readBoolean(): Boolean = buffer.get != 0

  final def readString(): String = {
    val length = buffer.getInt
    val b = new StringBuffer(length)
    for (i ← 1 to length) b.append(iso88591ByteToChar(buffer.get))
    b.toString
  }

  final def readUUID(): UUID = {
    val high = buffer.getLong
    val low = buffer.getLong
    new UUID(high, low)
  }
}

private object BaseDeserializer {
  private def iso88591ByteToChar(o: Byte) = (o.toInt & 0xFF).toChar  // ISO-8859-1
}

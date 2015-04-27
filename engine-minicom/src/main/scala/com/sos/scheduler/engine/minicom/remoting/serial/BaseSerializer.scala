package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.remoting.serial.BaseSerializer._
import java.nio.ByteBuffer
import java.util.UUID
import scala.math.max

/**
 * @author Joacim Zschimmer
 */
private[serial] class BaseSerializer {
  private var byteBuffer = ByteBuffer allocate InitialSize

  final def writeByte(o: Byte): Unit = {
    need(1)
    byteBuffer.put(o)
  }

  final def writeInt32(o: Int): Unit = {
    need(4)
    byteBuffer.putInt(o)
  }

  final def writeInt64(o: Long): Unit = {
    need(8)
    byteBuffer.putLong(o)
  }

  final def writeDouble(o: Double): Unit = {
    val string = o.toString
    need(2)
    byteBuffer.put('s'.toByte)
    byteBuffer.put(string.length.toByte)
    need(string.length)
    for (o ← string) byteBuffer.put(o.toByte)
  }

  final def writeBoolean(o: Boolean): Unit = {
    need(1)
    byteBuffer.put(if (o) 1.toByte else 0.toByte)
  }

  final def writeString(o: String): Unit = {
    need(4 + o.length)
    byteBuffer.putInt(o.length)
    for (c ← o.iterator) byteBuffer.put(charToIso88591Byte(c))
  }

  final def writeUUID(o: UUID): Unit = byteBuffer.putLong(o.getMostSignificantBits).putLong(o.getLeastSignificantBits)

  final def need(n: Int): Unit = {
    val neededSize = byteBuffer.position + n
    if (neededSize > byteBuffer.limit) {
      val b = ByteBuffer.allocate(increased(byteBuffer.limit, neededSize))
      b.put(byteBuffer.array, byteBuffer.arrayOffset, byteBuffer.position)
      byteBuffer = b
    }
  }

  def byteArrayAndLength = (byteBuffer.array, byteBuffer.position)
}

private object BaseSerializer {
  private[remoting] val InitialSize = 1000
  private def charToIso88591Byte(o: Char) = o.toByte    // ISO-8859-1

  private[remoting] def increased(currentSize: Int, neededSize: Int) =
    max(2 * currentSize, neededSize + InitialSize)
}

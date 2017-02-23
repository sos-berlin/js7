package com.sos.scheduler.engine.minicom.remoting.serial

import akka.util.ByteStringBuilder
import com.sos.scheduler.engine.minicom.remoting.serial.BaseSerializer._
import java.nio.ByteOrder
import java.nio.ByteOrder.BIG_ENDIAN
import java.util.UUID

/**
 * @author Joacim Zschimmer
 */
private[serial] class BaseSerializer {
  private val builder = new ByteStringBuilder

  private implicit def byteOrder: ByteOrder = BIG_ENDIAN

  final def writeByte(o: Byte): Unit = builder.putByte(o)

  final def writeInt16(o: Short): Unit = builder.putShort(o)

  final def writeInt32(o: Int): Unit = builder.putInt(o)

  final def writeInt64(o: Long): Unit = builder.putLong(o)

  final def writeDouble(o: Double): Unit = {
    val string = o.toString
    builder.putByte('s'.toByte)
    builder.putByte(string.length.toByte)
    for (o ← string) builder.putByte(o.toByte)
  }

  final def writeBoolean(o: Boolean): Unit = builder.putByte(if (o) 1.toByte else 0.toByte)

  final def writeString(o: String): Unit = {
    builder.putInt(o.length)
    for (c ← o.iterator) builder.putByte(charToIso88591Byte(c))
  }

  final def writeUUID(o: UUID): Unit = builder.putLong(o.getMostSignificantBits).putLong(o.getLeastSignificantBits)

  def toByteString = builder.result()
}

private object BaseSerializer {
  private def charToIso88591Byte(o: Char) = o.toByte    // ISO-8859-1
}

package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.remoting.serial.BaseDeserializer._
import com.sos.scheduler.engine.minicom.types.{EXCEPINFO, HRESULT}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.US_ASCII
import java.util.UUID

/**
 * @author Joacim Zschimmer
 */
private[serial] trait BaseDeserializer {

  protected val buffer: ByteBuffer

  final def requireEndOfMessage(): Unit =
    buffer.remaining match {
      case 0 ⇒
      case n ⇒ throw new IllegalArgumentException(s"Message contains $n more bytes than expected")
    }

  final def hasData = buffer.remaining > 0

  final def readExcepInfo(): EXCEPINFO = {
    /*val code =*/ readInt16()
    /*val reserved =*/ readInt16()
    val source = readString()
    val description = readString()
    /*val helpFile =*/ readString()
    /*val helpContext =*/ readInt32()
    /*val sCode =*/ HRESULT(readInt32())
    EXCEPINFO(source = source, description = description)
  }

  final def readInt16(): Short = buffer.getShort

  final def readInt32(): Int = buffer.getInt

  final def readInt64(): Long = buffer.getLong

  final def readDouble(): Double = {
    require(readByte() == 's')
    val length = readByte()
    require(length > 0)
    val array = new Array[Byte](length)
    buffer.get(array)
    new String(array, US_ASCII).toDouble
  }

  final def readByte(): Byte = buffer.get

  final def readBoolean(): Boolean = buffer.get != 0

  final def readString(): String = {
    val length = buffer.getInt
    val b = new StringBuilder(length)
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

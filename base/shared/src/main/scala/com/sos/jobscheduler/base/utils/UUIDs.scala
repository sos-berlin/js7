package com.sos.jobscheduler.base.utils

import java.nio.ByteBuffer
import java.util.{Base64, UUID}

object UUIDs
{
  def uuidToBase64(uuid: UUID): String = {
    val buffer = ByteBuffer.wrap(new Array[Byte](16))
    buffer.putLong(uuid.getMostSignificantBits)
    buffer.putLong(uuid.getLeastSignificantBits)
    Base64.getUrlEncoder.encodeToString(buffer.array) stripSuffix "=="
  }

  def base64ToUUID(base64String: String): UUID = {
    val bytes = Base64.getUrlDecoder.decode(base64String)
    if (bytes.size != 16) throw new IllegalArgumentException("UUID is not 16 bytes long")
    val buffer = ByteBuffer.wrap(bytes)
    new UUID(buffer.getLong(0), buffer.getLong(8))
  }
}

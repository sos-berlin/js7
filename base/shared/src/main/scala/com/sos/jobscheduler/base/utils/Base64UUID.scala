package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import java.nio.ByteBuffer
import java.util.{Base64, UUID}

final case class Base64UUID private(uuid: UUID, string: String) extends GenericString

object Base64UUID extends GenericString.Checked_[Base64UUID]
{
  def apply(uuid: UUID) = new Base64UUID(uuid, uuidToBase64(uuid))

  protected def unchecked(string: String) = throw new NotImplementedError

  override def checked(string: String) =
    for (uuid <- base64ToUUID(string)) yield new Base64UUID(uuid, string)

  def uuidToBase64(uuid: UUID): String = {
    val buffer = ByteBuffer.wrap(new Array[Byte](16))
    buffer.putLong(uuid.getMostSignificantBits)
    buffer.putLong(uuid.getLeastSignificantBits)
    Base64.getUrlEncoder.encodeToString(buffer.array) stripSuffix "=="
  }

  def base64ToUUID(base64String: String): Checked[UUID] =
    for {
      bytes <- Checked.catchNonFatal { Base64.getUrlDecoder.decode(base64String) }
      uuid <-
        if (bytes.size != 16)
          Left(Problem(s"Not a Base64-encoded UUID: $base64String"))
        else {
          val buffer = ByteBuffer.wrap(bytes)
          Right(new UUID(buffer.getLong(0), buffer.getLong(8)))
        }
    } yield uuid
}

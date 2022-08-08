package js7.base.utils

import java.nio.ByteBuffer
import java.util.{Base64, UUID}
import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}

final case class Base64UUID private(uuid: UUID, string: String) extends GenericString

object Base64UUID extends GenericString.Checked_[Base64UUID]
{
  val zero = apply(new UUID(0, 0))
  val ffff = apply(new UUID(-1, -1))

  def apply(uuid: UUID) = new Base64UUID(uuid, uuidToBase64(uuid))

  def random() = apply(UUID.randomUUID())

  def randomString(): String = uuidToBase64(UUID.randomUUID())

  protected def unchecked(string: String) = throw new NotImplementedError

  override def checked(string: String) =
    for (uuid <- base64ToUUID(string)) yield new Base64UUID(uuid, string)

  private lazy val toUrlBase64 = Base64.getUrlEncoder.withoutPadding.encodeToString _

  def uuidToBase64(uuid: UUID): String = {
    val buffer = ByteBuffer.wrap(new Array[Byte](16))
    buffer.putLong(uuid.getMostSignificantBits)
    buffer.putLong(uuid.getLeastSignificantBits)
    toUrlBase64(buffer.array)
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

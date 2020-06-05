package js7.data.event

import js7.base.generic.GenericString
import js7.base.utils.Base64UUID
import java.util.UUID
import java.util.UUID.randomUUID

/** The ID of a journal, identifying the event stream. */
final case class JournalId private(base64UUID: Base64UUID) extends GenericString {
  def string = base64UUID.string
}

object JournalId extends GenericString.Checked_[JournalId]
{
  def apply(uuid: UUID) = new JournalId(Base64UUID(uuid))

  def random() = JournalId(randomUUID)

  protected def unchecked(string: String) = throw new NotImplementedError

  override def checked(string: String) =
    for (o <- Base64UUID.checked(string)) yield new JournalId(o)
}

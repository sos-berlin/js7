package js7.data.event

import java.util.UUID
import java.util.UUID.randomUUID
import js7.base.generic.GenericString
import js7.base.problem.Checked
import js7.base.utils.Base64UUID

/** The ID of a journal, identifying the event stream. */
final case class JournalId(base64UUID: Base64UUID) extends GenericString:
  def string: String = base64UUID.string


object JournalId extends GenericString.Checked_[JournalId]:
  def apply(uuid: UUID) = new JournalId(Base64UUID(uuid))

  def random(): JournalId = JournalId(randomUUID)

  protected def unchecked(string: String): JournalId = throw new NotImplementedError

  override def checked(string: String): Checked[JournalId] =
    for o <- Base64UUID.checked(string) yield new JournalId(o)

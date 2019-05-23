package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.utils.Base64UUID
import java.util.UUID

/** The ID of a journal, identifying the event stream. */
final case class JournalId private(base64UUID: Base64UUID) extends GenericString {
  def string = base64UUID.string
}

object JournalId extends GenericString.Checked_[JournalId]
{
  def apply(uuid: UUID) = new JournalId(Base64UUID(uuid))

  protected def unchecked(string: String) = throw new NotImplementedError

  override def checked(string: String) =
    for (o <- Base64UUID.checked(string)) yield new JournalId(o)
}

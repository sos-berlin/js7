package js7.data.controller

import js7.base.generic.GenericString
import js7.base.problem.Checked
import js7.base.utils.Base64UUID
import js7.data.event.JournalId

/** The ID of an Controller run.
  * It identifies an Controller's event stream.
  * So and Controllers keeps its `ControllerRunId` event if its restartet,
  * as long as it uses the same event steam (Journal). */
final case class ControllerRunId(journalId: JournalId) extends GenericString:
  def string: String = journalId.string

  override def toString: String = typedToString


object ControllerRunId extends GenericString.NonEmpty[ControllerRunId]:
  val empty: ControllerRunId =
    ControllerRunId(JournalId(Base64UUID.zero))

  protected def unchecked(string: String): ControllerRunId =
    throw new NotImplementedError

  override def checked(string: String): Checked[ControllerRunId] =
    JournalId.checked(string).map(apply)

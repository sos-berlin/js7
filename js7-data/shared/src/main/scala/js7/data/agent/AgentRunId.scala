package js7.data.agent

import js7.base.generic.GenericString
import js7.base.utils.Base64UUID
import js7.data.event.JournalId

/** The ID of an Agent run.
  * It identifies an Agent's event stream.
  * So and Agents keeps its `AgentRunId` event if its restartet,
  * as long as it uses the same event steam (Journal). */
final case class AgentRunId(journalId: JournalId) extends GenericString
{
  def string = journalId.string

  override def toString = typedToString
}

object AgentRunId extends GenericString.NonEmpty[AgentRunId]
{
  val empty = AgentRunId(JournalId(Base64UUID.zero))

  protected def unchecked(string: String) = throw new NotImplementedError

  override def checked(string: String) = JournalId.checked(string) map apply
}

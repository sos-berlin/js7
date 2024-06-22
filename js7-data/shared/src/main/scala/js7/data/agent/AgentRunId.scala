package js7.data.agent

import js7.base.generic.GenericString
import js7.base.problem.Checked
import js7.base.utils.Base64UUID
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.event.JournalId

/** The ID of an Agent run.
  * It identifies an Agent's event stream.
  * So and Agents keeps its `AgentRunId` event if its restartet,
  * as long as it uses the same event steam (Journal). */
final case class AgentRunId(journalId: JournalId) extends GenericString:

  def string: String = journalId.string

  override def toString: String = typedToString


object AgentRunId extends GenericString.NonEmpty[AgentRunId]:
  val empty: AgentRunId = AgentRunId(JournalId(Base64UUID.zero))
  val pastAgentVersion: AgentRunId = checked("--PAST-AGENT-VERSION--").orThrow

  protected def unchecked(string: String): Nothing =
    throw new NotImplementedError

  override def checked(string: String): Checked[AgentRunId] =
    JournalId.checked(string) map apply

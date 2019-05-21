package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.base.generic.GenericString

/** The ID of an Agent run.
  * It identifies an Agent's event stream.
  * So and Agents keeps its `AgentRunId` event if its restartet,
  * as long as it uses the same event steam (Journal). */
final case class AgentRunId(string: String) extends GenericString

object AgentRunId extends GenericString.NonEmpty[AgentRunId]
{
  protected def unchecked(string: String): AgentRunId = new AgentRunId(string)
}

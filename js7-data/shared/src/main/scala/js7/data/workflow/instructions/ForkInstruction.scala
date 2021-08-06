package js7.data.workflow.instructions

import js7.data.agent.AgentPath
import js7.data.workflow.Instruction

trait ForkInstruction extends Instruction
{
  def agentPath: Option[AgentPath]
}

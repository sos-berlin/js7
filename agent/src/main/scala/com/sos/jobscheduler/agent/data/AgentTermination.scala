package js7.agent.data

sealed trait AgentTermination

object AgentTermination
{
  final case class Terminate(restart: Boolean = false) extends AgentTermination
}

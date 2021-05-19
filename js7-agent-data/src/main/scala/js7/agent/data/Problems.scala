package js7.agent.data

import js7.base.problem.Problem
import js7.data.agent.AgentPath
import js7.data.order.OrderId

/**
  * @author Joacim Zschimmer
  */
object Problems
{
  case object AgentIsShuttingDown extends Problem.ArgumentlessCoded {
    override def httpStatusCode = 503  // Service unavailable
  }

  final case class AgentRunIdMismatchProblem(agentPath: AgentPath)
  extends Problem.Coded {
    def arguments = Map("agentPath" -> agentPath.string)
  }

  final case class AgentPathMismatchProblem(expectedAgentPath: AgentPath, realAgentPath: AgentPath)
  extends Problem.Coded {
    def arguments = Map(
      "expectedAgentPath" -> expectedAgentPath.string,
      "realAgentPath" -> realAgentPath.string)
  }

  final case class AgentDuplicateOrder(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" -> orderId.string)
  }

  case object AgentNotCreatedProblem extends Problem.ArgumentlessCoded
}

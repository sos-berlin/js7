package js7.agent.data

import js7.base.problem.Problem
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.order.OrderId

/**
  * @author Joacim Zschimmer
  */
object Problems
{
  case object AgentIsShuttingDown extends Problem.ArgumentlessCoded {
    override val httpStatusCode = 503  // Service unavailable
  }

  final case class AgentRunIdMismatchProblem(agentPath: AgentPath)
  extends Problem.Coded {
    def arguments = Map("agentPath" -> agentPath.string)
  }

  final case class AgentPathMismatchProblem(requestedAgentPath: AgentPath, realAgentPath: AgentPath)
  extends Problem.Coded {
    def arguments = Map(
      "requestedAgentPath" -> requestedAgentPath.string,
      "realAgentPath" -> realAgentPath.string)
  }

  final case class AgentWrongControllerProblem(requestedControllerId: ControllerId)
  extends Problem.Coded {
    def arguments = Map(
      "requestedControllerId" -> requestedControllerId.string)
  }

  final case class AgentDuplicateOrder(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" -> orderId.string)
  }

  case object AgentAlreadyCreatedProblem extends Problem.ArgumentlessCoded

  case object AgentNotCreatedProblem extends Problem.ArgumentlessCoded
}

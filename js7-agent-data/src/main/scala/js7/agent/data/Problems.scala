package js7.agent.data

import js7.base.problem.Problem
import js7.data.agent.AgentId
import js7.data.controller.ControllerId
import js7.data.order.OrderId

/**
  * @author Joacim Zschimmer
  */
object Problems
{
  case object AgentIsShuttingDown extends Problem.ArgumentlessCoded {
    override def httpStatusCode = 503  // Service unavailable
  }

  final case class ControllerAgentMismatch(agentId: AgentId)
  extends Problem.Coded {
    def arguments = Map("agentId" -> agentId.string)
  }

  final case class DuplicateAgentRef(first: AgentId, second: AgentId)
  extends Problem.Coded {
    def arguments = Map(
      "first" -> first.string,
      "second" -> second.string)
  }

  final case class AgentDuplicateOrder(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" -> orderId.string)
  }

  final case class UnknownController(controllerId: ControllerId) extends Problem.Coded {
    def arguments = Map("controllerId" -> controllerId.string)
  }

  case object SignedInjectionNotAllowed extends Problem.ArgumentlessCoded
}

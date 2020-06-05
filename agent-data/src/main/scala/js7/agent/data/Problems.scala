package js7.agent.data

import js7.base.problem.Problem
import js7.data.agent.AgentRefPath
import js7.data.master.MasterId
import js7.data.order.OrderId

/**
  * @author Joacim Zschimmer
  */
object Problems
{
  case object AgentIsShuttingDown extends Problem.ArgumentlessCoded {
    override def httpStatusCode = 503  // Service unavailable
  }

  final case class MasterAgentMismatch(agentRefPath: AgentRefPath)
  extends Problem.Coded {
    def arguments = Map("agentRefPath" -> agentRefPath.string)
  }

  final case class DuplicateAgentRef(first: AgentRefPath, second: AgentRefPath)
  extends Problem.Coded {
    def arguments = Map(
      "first" -> first.string,
      "second" -> second.string)
  }

  final case class AgentDuplicateOrder(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" -> orderId.string)
  }

  final case class UnknownMaster(masterId: MasterId) extends Problem.Coded {
    def arguments = Map("masterId" -> masterId.string)
  }

  case object SignedInjectionNotAllowed extends Problem.ArgumentlessCoded
}

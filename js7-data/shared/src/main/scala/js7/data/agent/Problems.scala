package js7.data.agent

import js7.base.problem.Problem
import js7.data.controller.ControllerId
import js7.data.order.OrderId

/**
  * @author Joacim Zschimmer
  */
object Problems:
  case object AgentIsShuttingDown extends Problem.ArgumentlessCoded:
    override val httpStatusCode = 503  // Service unavailable

  final case class AgentRunIdMismatchProblem(agentPath: AgentPath)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "agentPath" -> agentPath.string)

  object AgentRunIdMismatchProblem extends Problem.Coded.Companion


  final case class AgentPathMismatchProblem(requestedAgentPath: AgentPath, realAgentPath: AgentPath)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "requestedAgentPath" -> requestedAgentPath.string,
      "realAgentPath" -> realAgentPath.string)


  final case class AgentWrongControllerProblem(
    requestedControllerId: ControllerId,
    realControllerId: ControllerId)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "requestedControllerId" -> requestedControllerId.string,
      "realControllerId" -> realControllerId.string)


  final case class AgentDuplicateOrder(orderId: OrderId) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "orderId" -> orderId.string)


  type AgentAlreadyDedicatedProblem = AgentAlreadyDedicatedProblem.type
  case object AgentAlreadyDedicatedProblem extends Problem.ArgumentlessCoded


  type AgentNotDedicatedProblem = AgentNotDedicatedProblem.type
  case object AgentNotDedicatedProblem extends Problem.ArgumentlessCoded

  //type AgentNotCoupledProblem = AgentNotCoupledProblem.type
  //case object AgentNotCoupledProblem extends Problem.ArgumentlessCoded

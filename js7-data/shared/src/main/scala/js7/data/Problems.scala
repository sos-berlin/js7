package js7.data

import js7.base.problem.Problem
import js7.data.order.OrderId

object Problems
{
  case object PassiveClusterNodeShutdownNotAllowedProblem extends Problem.ArgumentlessCoded

  final case class CancelChildOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" -> orderId.string)
  }

  final case class CancelStartedOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" -> orderId.string)
  }

  final case class UnknownOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" -> orderId.string)
  }
}

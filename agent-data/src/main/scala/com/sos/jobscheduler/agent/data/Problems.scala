package com.sos.jobscheduler.agent.data

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderId

/**
  * @author Joacim Zschimmer
  */
object Problems
{
  case object MasterAgentMismatchProblem extends Problem.ArgumentlessCoded

  final case class AgentDuplicateOrder(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" -> orderId.string)
  }

  final case class UnknownMaster(masterId: MasterId) extends Problem.Coded {
    def arguments = Map("masterId" -> masterId.string)
  }

  case object SignedInjectionNotAllowed extends Problem.ArgumentlessCoded
}

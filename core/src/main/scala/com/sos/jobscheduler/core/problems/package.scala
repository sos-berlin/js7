package com.sos.jobscheduler.core

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.OrderId

/**
  * @author Joacim Zschimmer
  */
package object problems
{
  final case class CancelChildOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" → orderId.string)
  }

  final case class CancelStartedOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" → orderId.string)
  }

  final case class UnknownOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" → orderId.string)
  }

  final case class JsonSeqFileClosedProblem(file: String) extends Problem.Coded {
    def arguments = Map("file" → file)
  }

  final case object FatEventServiceBusyProblem extends Problem.ArgumentlessCoded

  final case class ReverseKeepEventsProblem(requestedAfter: EventId, currentAfter: EventId) extends Problem.Coded {
    def arguments = Map(
      "requestedAfter" → requestedAfter.toString,
      "currentAfter" → currentAfter.toString)
  }

  case object PGPTamperedWithMessageProblem extends Problem.ArgumentlessCoded

  case object PGPMessageSignedByUnknownProblem extends Problem.ArgumentlessCoded
}

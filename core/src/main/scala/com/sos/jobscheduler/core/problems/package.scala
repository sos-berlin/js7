package com.sos.jobscheduler.core

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderId

/**
  * @author Joacim Zschimmer
  */
package object problems
{
  final case class CancelChildOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" -> orderId.string)
  }

  final case class CancelStartedOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" -> orderId.string)
  }

  final case class UnknownOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" -> orderId.string)
  }

  final case class JsonSeqFileClosedProblem(file: String) extends Problem.Coded {
    def arguments = Map("file" -> file)
  }

  final case class ReverseKeepEventsProblem(requestedAfter: EventId, currentAfter: EventId) extends Problem.Coded {
    def arguments = Map(
      "requestedAfter" -> requestedAfter.toString,
      "currentAfter" -> currentAfter.toString)
  }

  case object TamperedWithSignedMessageProblem extends Problem.ArgumentlessCoded

  case object MessageSignedByUnknownProblem extends Problem.ArgumentlessCoded

  final case class NoSuchMasterProblem(masterId: MasterId) extends Problem.Coded {
    def arguments = Map("masterId" -> masterId.string)
  }

  final case object JobSchedulerIsShuttingDownProblem extends Problem.ArgumentlessCoded

  final case object ClusterNodeIsStillStartingProblem extends Problem.ArgumentlessCoded

  final case object ClusterNodeIsNotActiveProblem extends Problem.ArgumentlessCoded

  final case class MissingPassiveClusterNodeHeartbeatProblem(passiveUri: Uri) extends Problem.Coded {
    override def arguments = Map("uri" -> passiveUri.toString)
  }

  final case class MissingActiveClusterNodeHeartbeatProblem(activeUri: Uri) extends Problem.Coded {
    override def arguments = Map("uri" -> activeUri.toString)
  }
}

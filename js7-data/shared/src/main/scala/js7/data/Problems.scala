package js7.data

import js7.base.problem.Problem
import js7.data.item.RepoEvent.ItemAddedOrChanged
import js7.data.item.{ItemId, ItemId_, TypedPath, VersionId}
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

  final case class ItemDeletedProblem(id: ItemId_) extends Problem.Coded {
    def arguments = Map("id" -> id.pretty)
  }

  final case class ItemVersionDoesNotMatchProblem(versionId: VersionId, itemId: ItemId[_ <: TypedPath]) extends Problem.Coded {
    def arguments = Map("versionId" -> versionId.string, "id" -> itemId.toString)
  }

  final case class EventVersionDoesNotMatchProblem(versionId: VersionId, event: ItemAddedOrChanged) extends Problem.Coded {
    def arguments = Map("versionId" -> versionId.string, "event" -> event.toShortString)
  }
}

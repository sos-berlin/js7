package js7.data

import js7.base.problem.Problem
import js7.data.agent.AgentPath
import js7.data.event.EventId
import js7.data.item.VersionedEvent.VersionedItemAddedOrChanged
import js7.data.item.{InventoryItemKey, InventoryItemPath, ItemPath, VersionId, VersionedItemId}
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

  final case class UnknownItemKeyProblem(itemKey: InventoryItemKey) extends Problem.Coded {
    def arguments = Map("key" -> itemKey.toString)
  }

  final case class UnknownItemPathProblem(itemPath: InventoryItemPath) extends Problem.Coded {
    def arguments = Map("path" -> itemPath.toString)
  }

  final case class UnknownOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map("orderId" -> orderId.string)
  }

  final case class MissingReferencedItemProblem(
    referencingItem: InventoryItemKey,
    referencedItem: InventoryItemPath)
  extends Problem.Coded {
    def arguments = Map(
      "referencingItem" -> referencingItem.toString,
      "referencedItem" -> referencedItem.toString)
  }

  final case class AgentResetProblem(agentPath: AgentPath) extends Problem.Coded {
    def arguments = Map("agentPath" -> agentPath.string)
  }

  final case class VersionedItemDeletedProblem(path: ItemPath) extends Problem.Coded {
    def arguments = Map("path" -> path.pretty)
  }

  final case class ItemVersionDoesNotMatchProblem(versionId: VersionId, itemId: VersionedItemId[_ <: ItemPath]) extends Problem.Coded {
    def arguments = Map("versionId" -> versionId.string, "id" -> itemId.toString)
  }

  final case class EventVersionDoesNotMatchProblem(versionId: VersionId, event: VersionedItemAddedOrChanged) extends Problem.Coded {
    def arguments = Map("versionId" -> versionId.string, "event" -> event.toShortString)
  }

  final case class SnapshotForUnknownEventIdProblem(eventId: EventId) extends Problem.Coded {
    def arguments = Map(
      "eventId" -> eventId.toString,
      "eventIdString" -> EventId.toString(eventId))
  }
  object SnapshotForUnknownEventIdProblem extends Problem.Coded.Companion

  final case class CannotRemoveChildOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map(
      "orderId" -> orderId.string)
  }

  final case class CannotRemoveWatchingOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map(
      "orderId" -> orderId.string)
  }

  final case class UnknownSignatureTypeProblem(typeName: String) extends Problem.Coded {
    def arguments = Map("typeName" -> typeName)
  }
}

package js7.data

import js7.base.problem.Problem
import js7.base.time.ScalaTime.RichDuration
import js7.data.agent.AgentPath
import js7.data.cluster.{ClusterCommand, ClusterState}
import js7.data.event.EventId
import js7.data.item.VersionedEvent.VersionedItemAddedOrChanged
import js7.data.item.{InventoryItemKey, InventoryItemPath, VersionId, VersionedItemId, VersionedItemPath}
import js7.data.node.NodeId
import js7.data.order.OrderId
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.FunctionCall
import scala.concurrent.duration.FiniteDuration

object Problems
{
  case object PassiveClusterNodeShutdownNotAllowedProblem extends Problem.ArgumentlessCoded

  final case class InvalidFunctionArgumentsProblem(call: FunctionCall) extends Problem.Coded {
    def arguments = Map(
      "function" -> call.name,
      "arguments" -> call.arguments.mkString(", "))
  }

  final case class EvaluationFailedProblem(name: String, expression: Expression, problem: Problem)
  extends Problem.Coded {
    def arguments = Map(
      "name" -> name,
      "expression" -> expression.toString,
      "problem" -> problem.toString)
  }

  case object RecursiveEvaluationProblem extends Problem.ArgumentlessCoded

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
    itemKey: InventoryItemKey,
    referencedItemKey: InventoryItemPath)
  extends Problem.Coded {
    def arguments = Map(
      "itemKey" -> itemKey.toString,
      "referencedItemPath" -> referencedItemKey.toString)
  }

  final case class ItemIsStillReferencedProblem(
    itemPath: InventoryItemPath,
    referencingItemKey: InventoryItemKey)
  extends Problem.Coded {
    def arguments = Map(
      "itemPath" -> itemPath.toString,
      "referencingItemKey" -> referencingItemKey.toString)
  }

  final case class AgentResetProblem(agentPath: AgentPath) extends Problem.Coded {
    def arguments = Map("agentPath" -> agentPath.string)
  }

  final case class VersionedItemRemovedProblem(path: VersionedItemPath) extends Problem.Coded {
    def arguments = Map("path" -> path.toString)
  }

  final case class ItemVersionDoesNotMatchProblem(versionId: VersionId, itemId: VersionedItemId[? <: VersionedItemPath]) extends Problem.Coded {
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

  final case class CannotDeleteChildOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map(
      "orderId" -> orderId.string)
  }

  final case class CannotDeleteWatchingOrderProblem(orderId: OrderId) extends Problem.Coded {
    def arguments = Map(
      "orderId" -> orderId.string)
  }

  case object ClusterNodeIsNotReadyProblem extends Problem.ArgumentlessCoded {
    override val httpStatusCode = 503  // ServiceUnavailable
  }

  case object ClusterNodeIsNotActiveProblem extends Problem.ArgumentlessCoded {
    override val httpStatusCode = 503  // ServiceUnavailable
  }

  case object BackupClusterNodeNotAppointed extends Problem.ArgumentlessCoded {
    override val httpStatusCode = 503  // ServiceUnavailable
  }

  final case class MissingPassiveClusterNodeHeartbeatProblem(
    passiveId: NodeId,
    duration: FiniteDuration)
  extends Problem.Coded {
    override def arguments = Map(
      "passiveId" -> passiveId.toString,
      "duration" -> duration.pretty,
    )
  }

  final case class ClusterCommandInapplicableProblem(command: ClusterCommand, clusterState: ClusterState)
  extends Problem.Coded {
    override def arguments = Map(
      "command" -> command.toString,
      "clusterState" -> clusterState.toString)
  }

  case object ClusterNodeIsNotBackupProblem extends Problem.ArgumentlessCoded

  case object PrimaryClusterNodeMayNotBecomeBackupProblem extends Problem.ArgumentlessCoded

  case object ClusterNodesAlreadyAppointed extends Problem.ArgumentlessCoded

  final case class ClusterSettingNotUpdatable(clusterState: ClusterState)
  extends Problem.Coded {
    def arguments = Map("clusterState" -> clusterState.toString)
  }
  object ClusterSettingNotUpdatable extends Problem.Coded.Companion

  case object PassiveClusterNodeResetProblem extends Problem.ArgumentlessCoded
}

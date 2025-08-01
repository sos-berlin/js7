package js7.data

import js7.base.problem.Problem
import js7.base.time.ScalaTime.RichDuration
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.cluster.{ClusterCommand, ClusterState}
import js7.data.event.{Event, EventDrivenState, EventId}
import js7.data.item.VersionedEvent.VersionedItemAddedOrChanged
import js7.data.item.{InventoryItemKey, InventoryItemPath, VersionId, VersionedItemId, VersionedItemPath}
import js7.data.node.NodeId
import js7.data.order.OrderId
import js7.data.plan.PlanId
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.FunctionCall
import scala.collection.immutable.Map.{Map1, Map2, Map3}
import scala.concurrent.duration.FiniteDuration

object Problems:
  case object PassiveClusterNodeShutdownNotAllowedProblem extends Problem.ArgumentlessCoded

  final case class InvalidFunctionArgumentsProblem(call: FunctionCall) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "function" -> call.name,
      "arguments" -> call.arguments.toList.flatten.mkString(", "))

  final case class EvaluationFailedProblem(name: String, expression: Expression, problem: Problem)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "name" -> name,
      "expression" -> expression.toString,
      "problem" -> problem.toString)

  case object RecursiveEvaluationProblem extends Problem.ArgumentlessCoded

  final case class CancelStartedOrderProblem(orderId: OrderId) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "orderId" -> orderId.string)

  final case class UnknownItemKeyProblem(itemKey: InventoryItemKey) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "key" -> itemKey.toString)

  final case class UnknownItemPathProblem(itemPath: InventoryItemPath) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "path" -> itemPath.toString)

  final case class UnknownOrderProblem(orderId: OrderId) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "orderId" -> orderId.string)

  trait KeyedEventProblem extends Problem.Coded:
    def key: Any

  final case class OrderCannotAttachedToPlanProblem(orderId: OrderId, reason: String) extends KeyedEventProblem:
    def key: OrderId = orderId
    def arguments: Map[String, String] = Map2(
      "orderId", orderId.string,
      "reason", reason)

  @deprecated
  final case class OrderWouldNotMatchChangedPlanSchemaProblem(orderId: OrderId, planId: PlanId)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map2(
      "orderId", orderId.string,
      "planId", planId.toString)

  final case class MissingReferencedItemProblem(
    itemKey: InventoryItemKey,
    referencedItemKey: InventoryItemPath)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "itemKey" -> itemKey.toString,
      "referencedItemPath" -> referencedItemKey.toString)

  final case class ItemIsStillReferencedProblem(
    itemPath: InventoryItemPath,
    referencingItemKey: InventoryItemKey,
    moreInfo: String = "")
  extends Problem.Coded:
    def arguments: Map[String, String] = Map3(
      "itemPath", itemPath.toString,
      "referencingItemKey", referencingItemKey.toString,
      "moreInfo", moreInfo)

  final case class AgentResetProblem(agentPath: AgentPath) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "agentPath" -> agentPath.string)

  final case class VersionedItemRemovedProblem(path: VersionedItemPath) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "path" -> path.toString)

  final case class ItemVersionDoesNotMatchProblem(versionId: VersionId, itemId: VersionedItemId[? <: VersionedItemPath]) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "versionId" -> versionId.string, "id" -> itemId.toString)

  final case class EventVersionDoesNotMatchProblem(versionId: VersionId, event: VersionedItemAddedOrChanged) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "versionId" -> versionId.string, "event" -> event.toShortString)

  final case class SnapshotForUnknownEventIdProblem(eventId: EventId) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "eventId" -> eventId.toString,
      "eventIdString" -> EventId.toString(eventId))
  object SnapshotForUnknownEventIdProblem extends Problem.Coded.Companion

  final case class CannotDeleteChildOrderProblem(orderId: OrderId) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "orderId" -> orderId.string)

  final case class CannotDeleteWatchingOrderProblem(orderId: OrderId) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "orderId" -> orderId.string)

  case object ClusterModuleShuttingDownProblem extends Problem.ArgumentlessCoded:
    override val httpStatusCode = 503 // Service Unavailable

  case object ClusterNodeIsNotReadyProblem extends Problem.ArgumentlessCoded:
    override val httpStatusCode = 503  // ServiceUnavailable

  case object ClusterNodeIsNotActiveProblem extends Problem.ArgumentlessCoded:
    override val httpStatusCode = 503  // ServiceUnavailable

  final case class ClusterSwitchOverButNotCoupledProblem(clusterState: ClusterState)
  extends Problem.Coded:
    def arguments = Map1("clusterState", clusterState.getClass.shortClassName)


  case object BackupClusterNodeNotAppointed extends Problem.ArgumentlessCoded:
    override val httpStatusCode = 503  // ServiceUnavailable

  final case class MissingPassiveClusterNodeHeartbeatProblem(
    passiveId: NodeId,
    duration: FiniteDuration)
  extends Problem.Coded:
    override def arguments: Map[String, String] = Map(
      "passiveId" -> passiveId.toString,
      "duration" -> duration.pretty,
    )

  final case class ClusterCommandInapplicableProblem(command: ClusterCommand, clusterState: ClusterState)
  extends Problem.Coded:
    override def arguments: Map[String, String] = Map(
      "command" -> command.toString,
      "clusterState" -> clusterState.toString)

  case object ClusterNodeIsNotBackupProblem extends Problem.ArgumentlessCoded

  case object PrimaryClusterNodeMayNotBecomeBackupProblem extends Problem.ArgumentlessCoded

  case object ClusterNodesAlreadyAppointed extends Problem.ArgumentlessCoded

  final case class ClusterSettingNotUpdatable(clusterState: ClusterState)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "clusterState" -> clusterState.toString)
  object ClusterSettingNotUpdatable extends Problem.Coded.Companion

  case object PassiveClusterNodeUrlChangeableOnlyWhenNotCoupledProblem extends Problem.ArgumentlessCoded

  case object PassiveClusterNodeResetProblem extends Problem.ArgumentlessCoded

  case object AckFromActiveClusterNodeProblem extends Problem.ArgumentlessCoded

  final case class GoOrderInapplicableProblem(orderId: OrderId) extends Problem.Coded:
    def arguments: Map[String, String] = Map1("orderId", orderId.toString)

  case object ClusterNodeHasBeenSwitchedOverProblem extends Problem.ArgumentlessCoded

  final case class NoActiveClusterNodeProblem(clusterStates: Seq[String]) extends Problem.Coded:
    def arguments: Map[String, String] =
      Map1("clusterStates", clusterStates.mkString(", "))
        .filter(_._2.nonEmpty)


  final case class OldEventIdProblem(eventId: EventId, tornOlder: FiniteDuration)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map2(
      "eventId", EventId.toString(eventId),
      "tornOlder", tornOlder.pretty)

  case object OldEventIdProblem extends Problem.ArgumentlessCoded

  final case class EventNotHandledHereProblem(event: Event, here: EventDrivenState.Companion[?])
  extends Problem.Coded:
    def arguments: Map[String, String] = Map2(
      "event", event.getClass.simpleScalaName,
      "here", here.name)

  final case class PlanIsClosedProblem(planId: PlanId)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map1(
      "planId", planId.toString)

  final case class PlanIsFinishedProblem(planId: PlanId)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map1(
      "planId", planId.toString)

  final case class PlanIsDeletedProblem(planId: PlanId)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map1(
      "planId", planId.toString)

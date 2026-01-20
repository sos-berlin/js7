package js7.data.state

import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.NotImplementedMap
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.BoardPath
import js7.data.calendar.CalendarPath
import js7.data.cluster.ClusterState
import js7.data.controller.{ControllerEngineState, ControllerId, ControllerStatePlanFunctions}
import js7.data.event.{Event, KeyedEvent}
import js7.data.item.{InventoryItem, InventoryItemKey, UnsignedItemKey, UnsignedItemState, UnsignedSimpleItemPath, UnsignedSimpleItemState}
import js7.data.lock.LockPath
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.plan.PlanId
import js7.data.state
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import scala.collection.{MapView, View}

trait TestEngineState[Self <: TestEngineState[Self]] extends EngineState_[Self]:
  this: Self =>

  def isAgent: Boolean
  def controllerId: ControllerId
  def idToOrder: Map[OrderId, Order[Order.State]]
  def idToWorkflow: Map[WorkflowId, Workflow]
  def keyToUnsignedItemState_ : Map[UnsignedItemKey, UnsignedItemState]

  def copyX(
    idToOrder: Map[OrderId, Order[Order.State]] = idToOrder,
    keyToUnsignedItemState_ : Map[UnsignedItemKey, UnsignedItemState] = keyToUnsignedItemState_)
  : Self

  def applyKeyedEvent(keyedEvent: KeyedEvent[Event]) =
    keyedEvent match
      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        applyOrderEvent(orderId, event)

      case _ => eventNotApplicable(keyedEvent)

  def items: View[InventoryItem] =
    keyToUnsignedItemState_.values.view.map(_.item) ++
      idToWorkflow.values

  def orders = idToOrder.values

  def workflowPathToId(workflowPath: WorkflowPath) =
    Left(Problem.pure("workflowPathToId is not implemented"))

  def keyToUnsignedItemState = keyToUnsignedItemState_.view

  final lazy val keyToItem: MapView[InventoryItemKey, InventoryItem] =
    new MapView[InventoryItemKey, InventoryItem]:
      def get(itemKey: InventoryItemKey): Option[InventoryItem] =
        itemKey match
          case WorkflowId.as(id) => idToWorkflow.get(id)
          case path: LockPath => keyToUnsignedItemState.get(path).map(_.item)
          case path: BoardPath => keyToUnsignedItemState.get(path).map(_.item)
          case path: CalendarPath => keyToUnsignedItemState.get(path).map(_.item)

      def iterator = items.map(o => o.key -> o).iterator

      override def values = items

  protected def addOrders(orders: Seq[Order[Order.State]] = Nil, allowClosedPlan: Boolean)
  : Checked[Self] =
    Left(Problem.pure("addOrders is not implemented"))

  protected def update_(
    updateOrders: Seq[Order[Order.State]],
    removeOrders: Seq[OrderId],
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState],
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath])
  : Checked[Self] =
    // Do not touch unused entries, they may be a NotImplementedMap
    var x = this
    if removeOrders.nonEmpty then x = x.copyX(idToOrder = idToOrder -- removeOrders)
    if updateOrders.nonEmpty then x = x.copyX(idToOrder = idToOrder ++ updateOrders.map(o => o.id -> o))
    // externalVanishedOrders ???
    if removeUnsignedSimpleItems.nonEmpty then
      x = x.copyX(keyToUnsignedItemState_ = keyToUnsignedItemState_ -- removeUnsignedSimpleItems)
    if addItemStates.nonEmpty then x = x.copyX(
      keyToUnsignedItemState_ = keyToUnsignedItemState_ ++ addItemStates.map(o => o.path -> o))
    Right(x)


object TestEngineState:

  def of(
    isAgent: Boolean,
    controllerId: ControllerId = ControllerId("CONTROLLER"),
    orders: Option[Iterable[Order[Order.State]]] = None,
    workflows: Option[Iterable[Workflow]] = None,
    itemStates: Iterable[UnsignedSimpleItemState] = Nil)
  : TestEngineState[?] =
    if isAgent then
      AgentTestEngineState.of(controllerId, orders,workflows, itemStates)
    else
      ControllerTestState.of(controllerId, orders,workflows, itemStates)


case class ControllerTestState(
  controllerId: ControllerId = ControllerId("CONTROLLER"),
  idToOrder: Map[OrderId, Order[Order.State]] = new NotImplementedMap,
  idToWorkflow: Map[WorkflowId, Workflow] = new NotImplementedMap,
  keyToUnsignedItemState_ : Map[UnsignedItemKey, UnsignedItemState] = Map.empty)
extends
  TestEngineState[ControllerTestState],
  ControllerEngineState[ControllerTestState],
  ControllerStatePlanFunctions[ControllerTestState]:

  def companion: ControllerTestState.type = ControllerTestState

  def isAgent = false
  def clusterState: ClusterState = ClusterState.Empty
  def statistics = EngineStateStatistics.empty

  def copyX(
    idToOrder: Map[OrderId, Order[Order.State]],
    keyToUnsignedItemState_ : Map[UnsignedItemKey, UnsignedItemState])
  : ControllerTestState =
    copy(
      idToOrder = idToOrder,
      keyToUnsignedItemState_ = keyToUnsignedItemState_)

  protected def addOrder(order: Order[Order.State], allowClosedPlan: Boolean)
  : Checked[ControllerTestState] =
    idToOrder.checkNoDuplicate(order.id).map: _ =>
      copy(idToOrder = idToOrder.updated(order.id, order))

  protected def onOrderPlanAttached(orderId: OrderId, planId: PlanId)
  : Checked[ControllerTestState] =
    Left(Problem.pure("onOrderPlanAttached is not implemented"))


object ControllerTestState extends EngineState.Companion[ControllerTestState]:

  def of(
    controllerId: ControllerId = ControllerId("CONTROLLER"),
    orders: Option[Iterable[Order[Order.State]]] = None,
    workflows: Option[Iterable[Workflow]] = None,
    itemStates: Iterable[UnsignedSimpleItemState] = Nil)
  : ControllerTestState =
    ControllerTestState(
      controllerId,
      idToOrder = orders.fold_(new NotImplementedMap, _.toKeyedMap(_.id)),
      idToWorkflow = workflows.fold_(new NotImplementedMap, _.toKeyedMap(_.id)),
      keyToUnsignedItemState_ = itemStates.toKeyedMap(_.path))


case class AgentTestEngineState(
  controllerId: ControllerId = ControllerId("CONTROLLER"),
  idToOrder: Map[OrderId, Order[Order.State]] = new NotImplementedMap,
  idToWorkflow: Map[WorkflowId, Workflow] = new NotImplementedMap,
  keyToUnsignedItemState_ : Map[UnsignedItemKey, UnsignedItemState] = Map.empty)
extends TestEngineState[AgentTestEngineState]:

  def companion: AgentTestEngineState.type = AgentTestEngineState

  def isAgent = true
  def clusterState: ClusterState.Empty.type = ClusterState.Empty
  def statistics = EngineStateStatistics.empty

  def copyX(
    idToOrder: Map[OrderId, Order[Order.State]],
    keyToUnsignedItemState_ : Map[UnsignedItemKey, UnsignedItemState])
  : AgentTestEngineState =
    copy(
      idToOrder = idToOrder,
      keyToUnsignedItemState_ = keyToUnsignedItemState_)


object AgentTestEngineState extends EngineState.Companion[AgentTestEngineState]:

  def of(
    controllerId: ControllerId = ControllerId("CONTROLLER"),
    orders: Option[Iterable[Order[Order.State]]] = None,
    workflows: Option[Iterable[Workflow]] = None,
    itemStates: Iterable[UnsignedSimpleItemState] = Nil)
  : AgentTestEngineState =
    AgentTestEngineState(
      controllerId,
      idToOrder = orders.fold_(new NotImplementedMap, _.toKeyedMap(_.id)),
      idToWorkflow = workflows.fold_(new NotImplementedMap, _.toKeyedMap(_.id)),
      keyToUnsignedItemState_ = itemStates.toKeyedMap(_.path))

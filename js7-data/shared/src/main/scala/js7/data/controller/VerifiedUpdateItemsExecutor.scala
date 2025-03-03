package js7.data.controller

import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import js7.base.log.Logger
import js7.base.problem.Problems.DuplicateKey
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.syntax.sequence
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichEither, RichPartialFunction}
import js7.data.Problems.OrderWouldNotMatchChangedPlanSchemaProblem
import js7.data.agent.AgentPath
import js7.data.board.BoardItem
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.crypt.SignedItemVerifier
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, KeyedEvent, NoKeyEvent}
import js7.data.item.BasicItemEvent.{ItemDeleted, ItemDeletionMarked}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemAddedOrChanged, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemAddedOrChanged, UnsignedSimpleItemChanged}
import js7.data.item.VersionedEvent.{VersionedItemChanged, VersionedItemRemoved}
import js7.data.item.{BasicItemEvent, InventoryItem, InventoryItemEvent, InventoryItemPath, ItemRevision, SignableSimpleItem, SimpleItemPath, UnsignedSimpleItem, VersionedEvent, VersionedItemPath}
import js7.data.order.Order.ExpectingNotices
import js7.data.order.OrderEvent
import js7.data.order.OrderEvent.{OrderPlanAttached, OrderStateReset}
import js7.data.plan.PlanEvent.PlanDeleted
import js7.data.plan.{PlanSchema, PlanSchemaId, PlanSchemaState}
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowControlId, WorkflowId, WorkflowPath, WorkflowPathControl, WorkflowPathControlPath}
import scala.collection.View

object VerifiedUpdateItemsExecutor:
  /* TODO Delete (and add?) along the dependency tree
     to allow simultaneous deletion of interdependent items.
    OrderWatch (detached) ->
      AgentPath <-> SubagentItem ->
      Workflow ->
        AgentPath <-> SubagentItem ->
        GlobalBoard ->
        Calendar ->
        JobResource ->
        Lock

     Ordered by dependency;
     OrderWatch, Workflow, (AgentPath <-> SubagentItem), GlobalBoard, Calendar, JobResource, Lock

     Delete in the reverse order of addition?
   */
  private val logger = Logger[this.type]

  def execute(
    verifiedUpdateItems: VerifiedUpdateItems,
    controllerState: ControllerState,
    checkItem: PartialFunction[InventoryItem, Checked[Unit]] = PartialFunction.empty)
  : Checked[Seq[AnyKeyedEvent]] =
    def result: Checked[Seq[AnyKeyedEvent]] =
      (for
        versionedEvents <- versionedEvents(controllerState)
        updatedState <- controllerState.applyKeyedEvents(versionedEvents)
        simpleItemEvents <- simpleItemEvents(updatedState)
        updatedState <- updatedState.applyKeyedEvents(simpleItemEvents)
        updatedState <- updatedState.applyKeyedEvents:
          versionedEvents.view
            .collect { case KeyedEvent(_, e: VersionedItemRemoved) => e.path }
            .flatMap(deleteRemovedVersionedItem(_, updatedState))
        updatedState <- updatedState.applyKeyedEvents:
          versionedEvents.view
            .collect { case KeyedEvent(_, e: VersionedItemChanged) => e.path }
            .flatMap(controllerState.repo.pathToId)
            .filterNot(controllerState.isInUse)
            .map(previousItemId => NoKey <-: ItemDeleted(previousItemId))
        derivedWorkflowPathControlEvents = toDerivedWorkflowPathControlEvents(updatedState)
        updatedState <- updatedState.applyKeyedEvents(derivedWorkflowPathControlEvents)
        derivedWorkflowControlEvents = toDerivedWorkflowControlEvents(updatedState)
        updatedState <- updatedState.applyKeyedEvents(derivedWorkflowControlEvents)
        _ <- checkVerifiedUpdateConsistency(verifiedUpdateItems, updatedState)
        (orderPlanAttached, updatedState) <-
          if PlanSchema.DerivePlanFromOrderId && verifiedUpdateItems.hasPlanSchema then
            attachPlanlessOrders(updatedState)
          else
            Right(Nil -> controllerState)
      yield
        simpleItemEvents
          .concat(versionedEvents)
          .concat(derivedWorkflowPathControlEvents)
          .concat(derivedWorkflowControlEvents)
          .concat(orderPlanAttached)
          .toVector
      ).left.map:
        case prblm @ Problem.Combined(Seq(_, duplicateKey: DuplicateKey)) =>
          logger.debug(prblm.toString)
          duplicateKey
        case o => o

    def versionedEvents(controllerState: ControllerState)
    : Checked[Seq[KeyedEvent[VersionedEvent]]] =
      verifiedUpdateItems.maybeVersioned match
        case None => Right(Nil)
        case Some(versioned) =>
          controllerState.repo
            .itemsToEvents(
              versioned.versionId,
              versioned.verifiedItems.map(_.signedItem),
              versioned.remove)
            .map(_.map(NoKey <-: _))

    def simpleItemEvents(controllerState: ControllerState)
    : Checked[View[KeyedEvent[NoticeDeleted | PlanDeleted | InventoryItemEvent]]] =
      import verifiedUpdateItems.simple
      simple.verifiedSimpleItems
        .traverse:
          verifiedSimpleItemToEvent(_, controllerState)
        .flatMap: signedEvents =>
          simple.unsignedSimpleItems
            .traverse:
              unsignedSimpleItemToEvent(_, controllerState)
            .traverse: unsignedEvents =>
              // Check again, is deletedAgents necessary ???
              val deletedAgents = simple.delete.view.collect { case a: AgentPath => a }.toSet
              simple.delete
                .traverse:
                  simpleItemDeletionEvents(_, deletedAgents, controllerState)
                .map: events =>
                  events.flatten ++
                    (signedEvents ++ unsignedEvents).map(NoKey <-: _)
          .flatten
          .map(_.view)

    def toDerivedWorkflowPathControlEvents(controllerState: ControllerState)
    : View[KeyedEvent[InventoryItemEvent]] =
      verifiedUpdateItems.maybeVersioned.view
        .flatMap(_.remove)
        .collect:
          case o: WorkflowPath => o
        .flatMap:
          toDerivedWorkflowPathControlEvent(controllerState, _)

    def toDerivedWorkflowPathControlEvent(controllerState: ControllerState, workflowPath: WorkflowPath)
    : Option[KeyedEvent[InventoryItemEvent]] =
      val path = WorkflowPathControlPath(workflowPath)
      (controllerState.keyTo(WorkflowPathControl).contains(path)
        && !controllerState.repo.pathToItems(Workflow).contains(workflowPath)
        && !controllerState.itemToAgentToAttachedState.contains(path)
      ).thenSome(NoKey <-: ItemDeleted(path))

    def toDerivedWorkflowControlEvents(controllerState: ControllerState)
    : View[KeyedEvent[InventoryItemEvent]] =
      verifiedUpdateItems.maybeVersioned.view
        .flatMap(_.remove)
        .collect:
          case o: WorkflowPath => o
        .flatMap:
          toDerivedWorkflowControlEvent(controllerState, _)

    def toDerivedWorkflowControlEvent(controllerState: ControllerState, workflowId: WorkflowId)
    : Option[KeyedEvent[InventoryItemEvent]] =
      val id = WorkflowControlId(workflowId)
      (controllerState.keyTo(WorkflowControl).contains(id)
        && controllerState.idToWorkflow.isDefinedAt(workflowId)
        && !controllerState.itemToAgentToAttachedState.contains(id)
      ).thenSome(NoKey <-: ItemDeleted(id))

    def verifiedSimpleItemToEvent(
      verified: SignedItemVerifier.Verified[SignableSimpleItem],
      controllerState: ControllerState)
    : Checked[SignedItemAddedOrChanged] =
      val item = verified.item
      if item.itemRevision.isDefined then
        Left(Problem.pure("ItemRevision is not accepted here"))
      else
        Right:
          controllerState.pathToSimpleItem.get(item.key) match
            case None =>
              SignedItemAdded(verified.signedItem.copy(
                value = item.withRevision(ItemRevision.Initial.some)))
            case Some(existing) =>
              SignedItemChanged:
                verified.signedItem.copy(
                  value = verified.signedItem.value
                    .withRevision(Some(existing.nextRevision)))

    def unsignedSimpleItemToEvent(
      item: UnsignedSimpleItem,
      controllerState: ControllerState)
    : Checked[UnsignedSimpleItemAddedOrChanged] =
      if item.itemRevision.isDefined then
        Left(Problem.pure("ItemRevision is not accepted here"))
      else
        checkItem.getOrElse(item, Checked.unit).flatMap: _ =>
          controllerState.pathToSimpleItem.get(item.key) match
            case None =>
              Right:
                UnsignedSimpleItemAdded(item.withRevision(ItemRevision.Initial.some))
            case Some(existing) =>
              if item.isInstanceOf[BoardItem] && existing.isInstanceOf[BoardItem]
                && item.getClass != existing.getClass
              then
                Left(Problem.pure("Type of BoardItem cannot be changed"))
              else if controllerState.deletionMarkedItems.contains(item.key) then
                Left(Problem.pure(s"${item.key} is marked as deleted and cannot be changed"))
              else
                item.match
                  case item: PlanSchema if PlanSchema.DerivePlanFromOrderId =>
                    checkOrderPlanIds(item, controllerState)
                  case _ => Checked.unit
                .map: _ =>
                  UnsignedSimpleItemChanged:
                    item.withRevision(Some(existing.nextRevision))

    @deprecated("Order.planId is no longer derived from OrderId")
    def checkOrderPlanIds(item: PlanSchema, controllerState: ControllerState): Checked[Unit] =
      controllerState.keyTo(PlanSchemaState).checked(item.id)
        .flatMap: planSchemaState =>
          checkOrdersMatchStillItsPlan(controllerState,
            planSchemaState.copy(item = item))

    @deprecated("Order.planId is no longer derived from OrderId")
    def checkOrdersMatchStillItsPlan(
      controllerState: ControllerState,
      planSchemaState: PlanSchemaState)
    : Checked[Unit] =
      planSchemaState.orderIds
        .map(controllerState.idToOrder.checked)
        .map:
          _.flatMap: order =>
            planSchemaState.item.evalOrderToPlanId(controllerState.toPlanOrderScope(order))
              .flatMap: maybePlanId =>
                ((!order.planId.isGlobal ? order.planId) == maybePlanId) !!
                  OrderWouldNotMatchChangedPlanSchemaProblem(order.id, order.planId)
        .sequence
        .map(_.combineAll)

    @deprecated("Order.planId is no longer derived from OrderId")
    def attachPlanlessOrders(controllerState: ControllerState)
    : Checked[(Vector[KeyedEvent[OrderStateReset | OrderPlanAttached]], ControllerState)] =
      controllerState.orders.view
        .toVector
        .traverse: order =>
          controllerState.evalOrderToPlanId(order).flatMap:
            case None => Right(Vector.empty)
            case Some(planId) =>
              if !order.planId.isGlobal then
                Left(Problem(s"${order.id} is in already in ${order.planId}"))
              else
                Right:
                  order.isState[ExpectingNotices].thenList(order.id <-: OrderStateReset)
                    ::: (order.id <-: OrderPlanAttached(planId)) :: Nil
        .map(_.flatten)
        .flatMap: events =>
          controllerState.applyKeyedEvents(events)
            .map(events -> _)

    def simpleItemDeletionEvents(
      path: SimpleItemPath,
      isDeleted: Set[AgentPath],
      controllerState: ControllerState)
    : Checked[View[KeyedEvent[NoticeDeleted | PlanDeleted | BasicItemEvent.ForClient]]] =
      path match
        case path: InventoryItemPath.AttachableToAgent
          if controllerState.itemToAgentToAttachedState.contains(path)
            && !isAttachedToDeletedAgentsOnly(path, isDeleted, controllerState) =>
          Right:
            (!controllerState.deletionMarkedItems.contains(path) ? ItemDeletionMarked(path)).view
              .concat:
                controllerState.detach(path)
              .map(NoKey <-: _)

        case planSchemaId: PlanSchemaId =>
          controllerState.keyTo(PlanSchemaState).checked(planSchemaId)
            .flatMap(_.checkIsDeletable)
            .as:
              View.Single(NoKey <-: ItemDeleted(path))

        case _ =>
          Right(View.Single:
            ItemDeleted(path))

    // If the deleted Item (a SubagentItem) is attached only to deleted Agents,
    // then we delete the Item without detaching.
    def isAttachedToDeletedAgentsOnly(
      path: SimpleItemPath,
      isDeleted: Set[AgentPath],
      controllerState: ControllerState)
    : Boolean =
      controllerState.itemToAgentToAttachedState
        .get(path).view.flatMap(_.keys)
        .forall(isDeleted)

    result
  end execute

  private def deleteRemovedVersionedItem(path: VersionedItemPath, controllerState: ControllerState)
  : Option[KeyedEvent[ItemDeleted]] =
    controllerState.repo
      .pathToVersionToSignedItems(path)
      .tail.headOption
      // Now we have the overridden item
      .flatMap(_.maybeSignedItem)
      .map(_.value.id)
      .flatMap: itemId =>
        !controllerState.isInUse(itemId) ?
          (NoKey <-: ItemDeleted(itemId))

  private def checkVerifiedUpdateConsistency(
    verifiedUpdateItems: VerifiedUpdateItems,
    controllerState: ControllerState)
  : Checked[Unit] =
    val newChecked = controllerState.checkAddedOrChangedItems(verifiedUpdateItems.addOrChangeKeys)
    val delSimpleChecked = controllerState
      .checkDeletedSimpleItems(verifiedUpdateItems.simple.delete.toSet)
    val delVersionedChecked = controllerState.checkRemovedVersionedItems(
      verifiedUpdateItems.maybeVersioned.view.flatMap(_.remove))
    newChecked
      .combineLeftOrRight(delSimpleChecked)
      .combineLeftOrRight(delVersionedChecked)

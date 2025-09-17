package js7.data.controller

import cats.syntax.foldable.*
import cats.syntax.traverse.*
import fs2.Stream
import js7.base.annotation.slow
import js7.base.auth.UserId
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.fs2utils.StreamExtensions.appendOne
import js7.base.log.Logger
import js7.base.problem.Checked.RichCheckedIterable
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.Collections.RichMap
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.MultipleLinesBracket.{Space, streamInBrackets}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.data.Problems.{ItemIsStillReferencedProblem, MissingReferencedItemProblem}
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRefStateEvent}
import js7.data.board.NoticeEvent.{NoticeDeleted, NoticeMoved, NoticePosted}
import js7.data.board.{BoardPath, BoardState, GlobalBoard, Notice, NoticeEvent, NoticeId, NoticePlace, PlannableBoard}
import js7.data.calendar.{Calendar, CalendarPath, CalendarState}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.ControllerTestEvent
import js7.data.controller.ControllerState.{WorkflowToOrders, logger}
import js7.data.event.EventCounter.EventCount
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{ClusterableState, Event, EventId, ItemContainer, JournalEvent, JournalHeader, JournalState, KeyedEvent, KeyedEventTypedJsonCodec, SignedItemContainer, SnapshotMeta, SnapshotableState}
import js7.data.item.BasicItemEvent.{ItemAttachedStateEvent, ItemDeleted, ItemDeletionMarked, ItemDetachable}
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable, Detached, NotDetached}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedItemEvent.{UnsignedItemAdded, UnsignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, ClientAttachments, InventoryItem, InventoryItemEvent, InventoryItemKey, InventoryItemPath, ItemAttachedState, ItemRevision, Repo, SignableItem, SignableItemKey, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, SimpleItem, SimpleItemPath, UnsignedItemEvent, UnsignedItemKey, UnsignedItemState, UnsignedSimpleItem, UnsignedSimpleItemEvent, UnsignedSimpleItemPath, UnsignedSimpleItemState, VersionedControl, VersionedControlId_, VersionedEvent, VersionedItemId_, VersionedItemPath}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.node.{NodeId, NodeName}
import js7.data.order.Order.ExpectingNotices
import js7.data.order.OrderEvent.{OrderNoticeAnnounced, OrderNoticeEvent, OrderNoticeExpected, OrderNoticePosted, OrderNoticePostedV2_3, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderNoticesRead, OrderPlanAttached, OrderTransferred}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatch, OrderWatchEvent, OrderWatchPath, OrderWatchState, OrderWatchStateHandler}
import js7.data.plan.{Plan, PlanEvent, PlanId, PlanKey, PlanSchema, PlanSchemaEvent, PlanSchemaId, PlanSchemaState}
import js7.data.state.{EngineStateStatistics, EventDrivenStateView}
import js7.data.subagent.SubagentItemStateEvent.{SubagentShutdown, SubagentShutdownStarted, SubagentShutdownV7}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentBundleState, SubagentId, SubagentItem, SubagentItemState, SubagentItemStateEvent}
import js7.data.system.ServerMeteringEvent
import js7.data.value.Value
import js7.data.workflow.instructions.ConsumeNotices
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowControlId, WorkflowId, WorkflowPath, WorkflowPathControl, WorkflowPathControlPath}
import scala.collection.{MapView, View}
import scala.util.chaining.scalaUtilChainingOps

/**
  * @author Joacim Zschimmer
  */
final case class ControllerState(
  eventId: EventId,
  standards: SnapshotableState.Standards,
  controllerMetaState: ControllerMetaState,
  keyToUnsignedItemState_ : Map[UnsignedItemKey, UnsignedItemState],
  repo: Repo,
  pathToSignedSimpleItem: Map[SignableSimpleItemPath, Signed[SignableSimpleItem]],
  agentAttachments: ClientAttachments[AgentPath],
  /** Used for OrderWatch to allow to attach it from Agent. */
  deletionMarkedItems: Set[InventoryItemKey],
  idToOrder: Map[OrderId, Order[Order.State]],
  statistics: EngineStateStatistics,
  workflowToOrders: WorkflowToOrders = WorkflowToOrders(Map.empty))
extends
  SignedItemContainer,
  EventDrivenStateView[ControllerState],
  ControllerStateView,
  ControllerEventDrivenStateView[ControllerState],
  OrderWatchStateHandler[ControllerState],
  ClusterableState[ControllerState],
  ControllerStatePlanFunctions[ControllerState]:

  override def toStringStream: Stream[fs2.Pure, String] =
    Stream.emit[fs2.Pure, String]("ControllerState:\n")
      .appendOne:
        "eventId=" + EventId.toString(eventId) + "\n"
      .pipeIf(standards.nonEmpty):
        _.appendOne:
          standards.toString + "\n"
      .pipeIf(controllerMetaState.nonEmpty):
        _.appendOne:
          controllerMetaState.toString + "\n"
      .pipeIf(keyToUnsignedItemState_.nonEmpty):
        _.appendOne:
          "keyToUnsignedItemState_=\n"
        .append:
          Stream.iterable(keyToUnsignedItemState_.values.toVector.sorted).flatMap:
            _.toStringStream.map("  " + _ + "\n")
      .pipeIf(repo.nonEmpty):
        _.appendOne:
          "repo=\n"
        .append:
          Stream.iterable(repo.toEvents).map(o => s"  $o\n")
      .pipeIf(pathToSignedSimpleItem.nonEmpty):
        _.append:
          streamInBrackets("pathToSignedSimpleItem", Space):
            pathToSignedSimpleItem.values.toVector.sortBy(_.value.key)
      .pipeIf(agentAttachments.nonEmpty): stream =>
        stream.appendOne:
          agentAttachments.toString + "\n"
        : Stream[fs2.Pure, String]
      .pipeIf(deletionMarkedItems.nonEmpty):
        _.append:
          streamInBrackets("deletionMarkedItems", Space)(deletionMarkedItems.toVector.sorted)
      .pipeIf(idToOrder.nonEmpty):
        _.append:
          streamInBrackets("orders", Space)(idToOrder.values.toVector.sorted)
      .pipeIf(workflowToOrders.nonEmpty):
        _.append:
          workflowToOrders.toStringStream
      .append:
        statistics.toStringStream

  def isAgent = false

  def controllerId: ControllerId =
    controllerMetaState.controllerId

  def companion: ControllerState.type =
    ControllerState

  def name: String =
    controllerId.toString

  def clusterNodeIdToName(nodeId: NodeId): Checked[NodeName] =
    controllerMetaState.controllerId.toUserId.flatMap: o =>
      NodeName.checked(o.string)

  def clusterNodeToUserId(nodeId: NodeId): Checked[UserId] =
    controllerId.check.map: _ =>
      controllerId.unsafeUserId

  def estimatedSnapshotSize: Int =
    1 +
    standards.snapshotSize +
    controllerMetaState.isDefined.toInt +
    repo.estimatedEventCount +
    keyToUnsignedItemState_.size +
    keyTo(OrderWatchState).values.view.map(_.estimatedSnapshotSize - 1).sum +
    keyTo(PlanSchemaState).values.view.map(_.estimatedSnapshotSize - 1).sum +
    pathToSignedSimpleItem.size +
    agentAttachments.estimatedSnapshotSize +
    deletionMarkedItems.size +
    idToOrder.size +
    statistics.estimatedSnapshotSize

  def toSnapshotStream: Stream[fs2.Pure, Any] =
    Stream(
      Stream.emit(SnapshotEventId(eventId)),
      standards.toSnapshotStream,
      Stream.iterable(controllerMetaState.isDefined ? controllerMetaState),
      Stream.iterable(keyTo(WorkflowPathControl).values).flatMap(_.toSnapshotStream),
      Stream.iterable(keyTo(WorkflowControl).values).flatMap(_.toSnapshotStream),
      Stream.iterable(keyTo(AgentRefState).values).flatMap(_.toSnapshotStream),
      Stream.iterable(keyTo(SubagentItemState).values).flatMap(_.toSnapshotStream),
      Stream.iterable(keyTo(SubagentBundleState).values).flatMap(_.toSnapshotStream),
      Stream.iterable(keyTo(LockState).values).flatMap(_.toSnapshotStream),
      Stream.iterable(keyTo(PlanSchemaState).values).flatMap(_.toSnapshotStream),
      Stream.iterable(keyTo(BoardState).values).flatMap(_.toSnapshotStream),
      Stream.iterable(keyTo(CalendarState).values).flatMap(_.toSnapshotStream),
      Stream.iterable(keyTo(OrderWatchState).values).flatMap(_.toSnapshotStream),
      Stream.iterable(pathToSignedSimpleItem.values).map(SignedItemAdded(_)),
      Stream.iterable(repo.toEvents),
      agentAttachments.toSnapshotStream,
      Stream.iterable(deletionMarkedItems.map(ItemDeletionMarked(_))),
      Stream.iterable(idToOrder.values),
      statistics.toSnapshotStream
    ).flatten

  def withEventId(eventId: EventId): ControllerState =
    copy(eventId = eventId)

  def withStandards(standards: SnapshotableState.Standards): ControllerState =
    copy(standards = standards)

  def withoutStatistics: ControllerState =
    copy(statistics = EngineStateStatistics.empty)

  def checkedBoardState(path: BoardPath): Checked[BoardState] =
    keyToUnsignedItemState_.checked(path).asInstanceOf[Checked[BoardState]]

  def applyKeyedEvent(keyedEvent: KeyedEvent[Event]): Checked[ControllerState] =
    keyedEvent.match
      case KeyedEvent(_: NoKey, ControllerEvent.ControllerInitialized(controllerId, intiallyStartedAt)) =>
        Right(copy(controllerMetaState = controllerMetaState.copy(
          controllerId = controllerId,
          initiallyStartedAt = intiallyStartedAt)))

      case KeyedEvent(_: NoKey, event: ControllerEvent.ControllerReady) =>
        Right(copy(controllerMetaState = controllerMetaState.copy(
          timezone = event.timezone)))

      case KeyedEvent(_: NoKey, _: ControllerEvent.ControllerShutDown) =>
        Right(this)

      case KeyedEvent(_: NoKey, ControllerEvent.ControllerTestEvent) =>
        Right(this)

      case KeyedEvent(_: NoKey, event: InventoryItemEvent) =>
        event match
          case event: UnsignedSimpleItemEvent =>
            event match
              case UnsignedSimpleItemAdded(item) =>
                item match
                  case addedAgentRef: AgentRef =>
                    addedAgentRef
                      .convertFromV2_1
                      .flatMap: (agentRef, maybeSubagentItem) =>
                        for
                          pathToItemState <-
                            keyToUnsignedItemState_.insert(agentRef.path, AgentRefState(agentRef))
                          pathToItemState <-
                            maybeSubagentItem match
                              case None => Right(pathToItemState)
                              case Some(subagentItem) => pathToItemState
                                .insert(subagentItem.id, SubagentItemState.initial(subagentItem))
                        yield copy(
                          keyToUnsignedItemState_ = pathToItemState)

                  case orderWatch: OrderWatch =>
                    ow.addOrderWatch(orderWatch.toInitialItemState)

                  case item: UnsignedSimpleItem =>
                    if item.path == PlanSchemaId.Global then
                      Left(Problem("The Global PlanSchema is predefined and cannot be added"))
                    else
                      for o <- keyToUnsignedItemState_.insert(item.path, item.toInitialItemState) yield
                        copy(keyToUnsignedItemState_ = o)

              case UnsignedSimpleItemChanged(item) =>
                item match
                  case changedAgentRef: AgentRef =>
                    changedAgentRef
                      .convertFromV2_1
                      .flatMap: (agentRef, maybeSubagentItem) =>
                        def checkIsExtending(a: AgentRef, b: AgentRef) =
                          (a.directors == b.directors
                            || a.directors.length == 1 && b.directors.length == 2
                            && a.directors.head == b.directors.head
                          ) !! Problem.pure("Agent Directors cannot not be changed")

                        for
                          agentRefState <- keyTo(AgentRefState).checked(agentRef.path)
                          _ <- checkIsExtending(agentRefState.agentRef, agentRef)
                          updatedAgentRef <- agentRefState.updateItem(agentRef)
                        yield
                          copy(
                            keyToUnsignedItemState_ = keyToUnsignedItemState_
                              .updated(agentRef.path, updatedAgentRef)
                              .pipeMaybe(maybeSubagentItem): (pathToItemState, changedSubagentItem) =>
                                // COMPATIBLE with v2.2.2
                                keyTo(SubagentItemState)
                                  .get(changedSubagentItem.id)
                                  .fold(pathToItemState): subagentItemState =>
                                    pathToItemState.updated(changedSubagentItem.id,
                                      subagentItemState.copy(
                                        subagentItem = subagentItemState.item
                                          .updateUri(changedSubagentItem.uri))))

                  case orderWatch: OrderWatch =>
                    ow.changeOrderWatch(orderWatch)

                  case item: UnsignedSimpleItem =>
                    for
                      _ <- checkChangedItem(item)
                      itemState <- keyToUnsignedItemState_.checked(item.path)
                      updated <- itemState.updateItem(item.asInstanceOf[itemState.companion.Item])
                    yield
                      copy(
                        keyToUnsignedItemState_ =
                          keyToUnsignedItemState_.updated(item.path, updated))

          case event: SignedItemEvent =>
            event match
              case SignedItemAdded(Signed(item, signedString)) =>
                item match
                  case jobResource: JobResource =>
                    pathToSignedSimpleItem
                      .insert(jobResource.path -> Signed(jobResource, signedString))
                      .map(o => copy(pathToSignedSimpleItem = o))

              case SignedItemChanged(Signed(item, signedString)) =>
                item match
                  case jobResource: JobResource =>
                    Right(copy(
                      pathToSignedSimpleItem = pathToSignedSimpleItem +
                        (jobResource.path -> Signed(jobResource, signedString))))

          case event: UnsignedItemEvent =>
            event match
              case UnsignedItemAdded(item: VersionedControl) =>
                keyToUnsignedItemState_
                  .insert(item.key, item.toInitialItemState)
                  .map(o => copy(keyToUnsignedItemState_ = o))

              case UnsignedItemChanged(item: VersionedControl) =>
                Right(copy(
                  keyToUnsignedItemState_ = keyToUnsignedItemState_
                    .updated(item.key, item.toInitialItemState)))

          case event: BasicItemEvent.ForClient =>
            event match
              case event: ItemAttachedStateEvent =>
                agentAttachments.applyEvent(event).map: o =>
                  copy(agentAttachments = o)

              case ItemDeletionMarked(itemKey) =>
                Right(copy(
                  deletionMarkedItems = deletionMarkedItems + itemKey))

              case event @ ItemDeleted(itemKey) =>
                val updated = copy(
                  deletionMarkedItems = deletionMarkedItems - itemKey,
                  agentAttachments = agentAttachments.applyItemDeleted(event))

                itemKey match
                  case WorkflowId.as(workflowId) =>
                    repo.deleteItem(workflowId).map: repo =>
                      updated.copy(
                        repo = repo)

                  case path: OrderWatchPath =>
                    updated.ow.removeOrderWatch(path)

                  case jobResourcePath: JobResourcePath =>
                    Right(updated.copy(
                      pathToSignedSimpleItem = pathToSignedSimpleItem - jobResourcePath))

                  case boardPath: BoardPath =>
                    Right(updated.copy(
                      keyToUnsignedItemState_ =
                        (updated.keyToUnsignedItemState_ - boardPath) ++
                          removeBoardInPlanSchemaStates(boardPath).toKeyedMap(_.id)))

                  case key @ (_: AgentPath | _: SubagentId | _: SubagentBundleId |
                              _: LockPath |
                              _: CalendarPath | _: BoardPath |
                              _: WorkflowPathControlPath | WorkflowControlId.as(_)) =>
                    Right(updated.copy(
                      keyToUnsignedItemState_ = keyToUnsignedItemState_ - key))

                  case key: PlanSchemaId =>
                    update(removeUnsignedSimpleItems = key :: Nil)

                  case _ =>
                    Left(Problem(s"A '${itemKey.companion.itemTypeName}' is not deletable"))

      case KeyedEvent(_: NoKey, event: VersionedEvent) =>
        repo.applyEvent(event).map: o =>
          copy(repo = o)

      case KeyedEvent(agentPath: AgentPath, event: AgentRefStateEvent) =>
        for
          agentRefState <- keyTo(AgentRefState).checked(agentPath)
          agentRefState <- agentRefState.applyEvent(event)
        yield copy(
          keyToUnsignedItemState_ = keyToUnsignedItemState_ + (agentPath -> agentRefState))

      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        applyOrderEvent(orderId, event)

      case KeyedEvent(boardPath: BoardPath, noticePosted: NoticePosted) =>
        updatePlannedBoard(noticePosted.planId / boardPath):
          _.addNotice(noticePosted.toNotice(boardPath))
        .map: planSchemaState =>
          copy(
            keyToUnsignedItemState_ = keyToUnsignedItemState_
              .updated(planSchemaState.id, planSchemaState))

      case KeyedEvent(boardPath: BoardPath, NoticeDeleted(plannedNoticeKey)) =>
        updatePlannedBoard(plannedNoticeKey.planId / boardPath):
          _.removeNotice(plannedNoticeKey.noticeKey)
        .map: planSchemaState =>
          copy(
            keyToUnsignedItemState_ = keyToUnsignedItemState_
              .updated(planSchemaState.id, planSchemaState))

      case KeyedEvent(boardPath: BoardPath, noticeMoved: NoticeMoved) =>
        import noticeMoved.{endOfLife, fromPlannedNoticeKey, toPlannedNoticeKey}
        if toPlannedNoticeKey.planId == fromPlannedNoticeKey.planId then
          Left(Problem(s"$keyedEvent: PlanIds must be different"))
        else
          for
            fromPlannedBoard <- maybePlannedBoard(fromPlannedNoticeKey.planId / boardPath).toRight:
              Problem(s"${fromPlannedNoticeKey.planId / boardPath} does not exist")
            noticePlace <- fromPlannedBoard.toNoticePlace.checked(fromPlannedNoticeKey.noticeKey)
            from <- updatePlannedBoard(fromPlannedNoticeKey.planId / boardPath):
              _.moveFromNoticePlace(fromPlannedNoticeKey.noticeKey)
            to <- updatePlannedBoard(toPlannedNoticeKey.planId / boardPath):
              _.moveToNoticePlace(toPlannedNoticeKey.noticeKey, noticePlace, endOfLife)
          yield
            copy(
              keyToUnsignedItemState_ = keyToUnsignedItemState_ ++
                View(from, to).map(o => o.path -> o))

      case KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent) =>
        ow.onOrderWatchEvent(orderWatchPath <-: event)

      case KeyedEvent(subagentId: SubagentId, event: SubagentItemStateEvent) =>
        event match
          case SubagentShutdownStarted | SubagentShutdown | SubagentShutdownV7
            if !keyToUnsignedItemState_.contains(subagentId) =>
            // May arrive when SubagentItem has been deleted
            Right(this)

          case _ =>
            for
              o <- keyTo(SubagentItemState).checked(subagentId)
              o <- o.applyEvent(event)
            yield copy(
              keyToUnsignedItemState_ = keyToUnsignedItemState_.updated(subagentId, o))

      case KeyedEvent(planSchemaId: PlanSchemaId, event: PlanSchemaEvent) =>
        for
          planSchemaState <- keyTo(PlanSchemaState).checked(planSchemaId)
          planSchemaState <- planSchemaState.applyEvent(event)
        yield
          copy(keyToUnsignedItemState_ =
            keyToUnsignedItemState_.updated(planSchemaId, planSchemaState))

      case KeyedEvent(planId: PlanId, event: PlanEvent) =>
        import planId.{planKey, planSchemaId}
        for
          planSchemaState <- keyTo(PlanSchemaState).checked(planSchemaId)
          planSchemaState <- planSchemaState.applyPlanEvent(planKey, event)
        yield
          copy(keyToUnsignedItemState_ =
            keyToUnsignedItemState_.updated(planSchemaId, planSchemaState))

      case KeyedEvent(_, ControllerTestEvent) =>
        Right(this)

      case _ => applyStandardEvent(keyedEvent)
    .map: controllerState =>
      controllerState.copy(
        statistics = controllerState.statistics.applyKeyedEvent(keyedEvent))

  override protected def applyOrderEvent(orderId: OrderId, event: OrderEvent)
  : Checked[ControllerState] =
    event match
      case event: OrderNoticeEvent =>
        for
          previousOrder <- idToOrder.checked(orderId)
          updatedOrder <- previousOrder.applyEvent(event)
          itemStates <- applyOrderNoticeEvent(previousOrder, event)
          controllerState <- update(
            updateOrders = updatedOrder :: Nil,
            addItemStates = itemStates)
        yield
          controllerState

      case OrderPlanAttached(planId) =>
        for
          self <- super.applyOrderEvent(orderId, event)
          // Move Order from GlobalPlan's to PlanId's PlanSchemaState
          planSchemaState <- self.keyTo(PlanSchemaState).checked(planId.planSchemaId)
          globalPlanSchemaState <- self.keyTo(PlanSchemaState).checked(PlanSchemaId.Global)
          planSchemaState <-
            planSchemaState.addOrder(planId.planKey, orderId, allowClosedPlan = true)
          self <- self.update(
            addItemStates =
              globalPlanSchemaState.removeOrder(PlanKey.Global, orderId)
                :: planSchemaState
                :: Nil)
        yield
          self

      case event: OrderTransferred =>
        super.applyOrderEvent(orderId, event).map: updated =>
          updated.copy(
            workflowToOrders = workflowToOrders
              .transferOrder(idToOrder(orderId), updated.idToOrder(orderId).workflowId))

      case _ =>
        super.applyOrderEvent(orderId, event)

  private def applyOrderNoticeEvent(order: Order[Order.State], event: OrderNoticeEvent)
  : Checked[Seq[PlanSchemaState | BoardState]] =
    val orderId = order.id
    event match
      case OrderNoticeAnnounced(noticeId) =>
        updatePlannedBoard(noticeId.plannedBoardId)(_.announceNotice(noticeId.noticeKey))
          .map(_ :: Nil)

      case OrderNoticePostedV2_3(notice) =>
        for
          boardState <- orderIdToBoardState(orderId)
          noticeId = PlanId.Global / boardState.path / notice.noticeKey
          planSchemaState <- updatePlannedBoard(noticeId.plannedBoardId):
            _.addNoticeV2_3(notice)
        yield
          planSchemaState :: Nil

      case OrderNoticePosted(noticeId, endOfLife) =>
        updatePlannedBoard(noticeId.plannedBoardId)(_.addNotice(Notice(noticeId, endOfLife)))
          .map(_ :: Nil)

      case OrderNoticeExpected(noticeKey) =>
        // COMPATIBLE with v2.3
        for
          boardState <- orderIdToBoardState(orderId)
          noticeId = PlanId.Global / boardState.path / noticeKey
          plannedBoard <- updatePlannedBoard(noticeId.plannedBoardId): plannedBoard =>
            Right:
              plannedBoard.addExpectation(noticeId.noticeKey, orderId)
        yield
          plannedBoard :: Nil

      case OrderNoticesExpected(noticeIds) =>
        updatePlannedBoards(noticeIds): (plannedBoard, noticeKey) =>
          Right:
            plannedBoard.addExpectation(noticeKey, orderId)

      case OrderNoticesRead =>
        order.ifState[ExpectingNotices] match
          case None => Right(Nil)
          case Some(previousOrder) => removeNoticeExpectation(previousOrder)

      case OrderNoticesConsumptionStarted(consumedNoticeIds) =>
        val isConsumption = consumedNoticeIds.toSet
        val allNoticeIds =
          order.ifState[ExpectingNotices].fold_(Vector.empty, _.state.noticeIds)
            .concat(consumedNoticeIds)
            .distinct
        assertThat(allNoticeIds == consumedNoticeIds) // For now, application of OrderNoticesConsumed requires all notices

        for
          _ <- allNoticeIds.checkUniquenessBy(_.boardPath)
          planSchemaStates <-
            allNoticeIds.groupBy(_.planSchemaId).toVector.traverse: (planSchemaId, noticeIds) =>
              for
                planSchemaState <- keyTo(PlanSchemaState).checked(planSchemaId)
                planSchemaState <-
                  noticeIds.scanLeft(Checked(planSchemaState)): (checkedPlanSchemaState, noticeId) =>
                    checkedPlanSchemaState.flatMap:
                      _.updatePlannedBoard(noticeId.planKey, noticeId.boardPath): plannedBoard =>
                        if isConsumption(noticeId) then
                          Right(plannedBoard.startConsumption(noticeId.noticeKey, orderId))
                        else
                          plannedBoard.removeExpectation(noticeId.noticeKey, orderId)
                  .last
              yield
                planSchemaState
          boardStates <-
            consumedNoticeIds.traverse: noticeId =>
              keyTo(BoardState).checked(noticeId.boardPath).map:
                _.pushConsumption(orderId, noticeId.plannedNoticeKey)
        yield
          planSchemaStates ++ boardStates

      case OrderNoticesConsumed(failed) =>
        for
          instr <-
            if failed then
              findInstructionInCallStack[ConsumeNotices](order.workflowPosition)
            else
              // When succeeding, the Order must be in the instruction block just below
              // the ConsumeNotice instruction.
              order.workflowPosition.checkedParent.flatMap(instruction_[ConsumeNotices])
          (boardStates, notices) <-
            instr.referencedBoardPaths.toVector.traverse: boardPath =>
              keyTo(BoardState).checked(boardPath).flatMap: boardState =>
                boardState.popConsumption(orderId).map: (boardState, plannedNoticeKey) =>
                  boardState -> boardState.path / plannedNoticeKey
            .map(_.unzip)
          planSchemaStates <-
            notices.groupBy(_.planSchemaId).toVector.traverse: (planSchemaId, noticeIds) =>
              for
                planSchemaState <- keyTo(PlanSchemaState).checked(planSchemaId)
                planSchemaState <-
                  noticeIds.scanLeft(Checked(planSchemaState)): (checkedPlanSchemaState, noticeId) =>
                    checkedPlanSchemaState.flatMap:
                      _.updatePlannedBoard(noticeId.planKey, noticeId.boardPath): plannedBoard =>
                        plannedBoard.finishConsumption(noticeId.noticeKey, succeeded = !failed)
                  .last
              yield
                planSchemaState
        yield
          planSchemaStates ++ boardStates

  /** The Agents for each WorkflowPathControl which have not attached the current itemRevision. */
  def workflowPathControlToIgnorantAgents: MapView[WorkflowPathControlPath, Set[AgentPath]] =
    new MapView[WorkflowPathControlPath, Set[AgentPath]]:
      private val pathToWorkflowPathControl = keyTo(WorkflowPathControl)

      def get(key: WorkflowPathControlPath): Option[Set[AgentPath]] =
        pathToWorkflowPathControl.get(key).map(attachableToAgents)

      def iterator: Iterator[(WorkflowPathControlPath, Set[AgentPath])] =
        pathToWorkflowPathControl.iterator.flatMap: (k, v) =>
          val agents = attachableToAgents(v)
          agents.nonEmpty ? (k -> agents)

      override def values: Iterable[Set[AgentPath]] =
        pathToWorkflowPathControl.values.view.map(attachableToAgents)

      private def attachableToAgents(workflowPathControl: WorkflowPathControl): Set[AgentPath] =
        itemToAgentToAttachedState
          .get(workflowPathControl.path)
          .view
          .flatMap(_.collect:
            case (agentPath, Attachable) => agentPath)
          .toSet

  /** The Agents for each InventoryItemKey which have not attached the current Item. */
  def itemToIgnorantAgents[I <: InventoryItem](I: InventoryItem.Companion[I])
  : MapView[I.Key, Set[AgentPath]] =
    filteredItemToIgnorantAgents[I.Key](_.companion eq I.Key)

  /** The Agents for each InventoryItemKey which have not attached the current Item. */
  private def filteredItemToIgnorantAgents[K <: InventoryItemKey](filter_ : InventoryItemKey => Boolean)
  : MapView[K, Set[AgentPath]] =
    new MapView[K, Set[AgentPath]]:
      def get(key: K): Option[Set[AgentPath]] =
        itemToAgentToAttachedState.get(key).flatMap(toAgents)

      def iterator: Iterator[(K, Set[AgentPath])] =
        itemToAgentToAttachedState.iterator
          .flatMap:
            case (k: K @unchecked, v) if filter_(k) =>
              toAgents(v).map(k -> _)
            case _ => None

      private def toAgents(agentToAttachedState: Map[AgentPath, ItemAttachedState])
      : Option[Set[AgentPath]] =
        val agents = agentToAttachedState
          .collect { case (agentPath, Attachable) => agentPath }
          .toSet
        agents.nonEmpty ? agents

  def orderToAvailableNotices(orderId: OrderId): Seq[Notice] =
    for
      order <- idToOrder.get(orderId).toVector
      noticeId <- orderToExpectedNotices(order)
      noticePlace <- maybeNoticePlace(noticeId)
      notice <- noticePlace.notice
    yield
      notice

  def orderToStillExpectedNotices(orderId: OrderId): Seq[NoticeId] =
    idToOrder.get(orderId).toVector.flatMap: order =>
      orderToExpectedNotices(order).filter: noticeId =>
        import noticeId.{boardPath, planKey, planSchemaId}
        keyTo(PlanSchemaState).get(planSchemaId).exists: planSchemaState =>
          planSchemaState.toPlan.get(planKey).exists: plan =>
            !plan.toPlannedBoard.get(boardPath).fold(false)(_.hasNotice(noticeId.noticeKey))

  private def orderToExpectedNotices(order: Order[Order.State]): Seq[NoticeId] =
    order.ifState[Order.ExpectingNotices]
      .toVector
      .flatMap(_.state.noticeIds)

  private def isGlobalBoard(boardPath: BoardPath): Boolean =
    keyTo(BoardState).get(boardPath).exists(_.isGlobal)

  protected def pathToOrderWatchState = keyTo(OrderWatchState)

  protected def updateOrderWatchStates(
    orderWatchStates: Seq[OrderWatchState],
    remove: Seq[OrderWatchPath])
  : Checked[ControllerState] =
    update(
      addItemStates = orderWatchStates,
      removeUnsignedSimpleItems = remove)

  protected def addOrders(orders: Seq[Order[Order.State]], allowClosedPlan: Boolean)
  : Checked[ControllerState] =
    ControllerState.addOrders(this, orders, allowClosedPlan = allowClosedPlan)

  protected def update_(
    updateOrders: Seq[Order[Order.State]] = Nil,
    removeOrders: Seq[OrderId] = Nil,
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState] = Nil,
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath] = Nil)
  : Checked[ControllerState] =
    ControllerState.update(this,
      updateOrders, removeOrders, externalVanishedOrders, addItemStates, removeUnsignedSimpleItems)

  /** The named values as seen at the current workflow position. */
  def orderNamedValues(orderId: OrderId): Checked[MapView[String, Value]] =
    for
      order <- idToOrder.checked(orderId)
      workflow <- repo.idTo(Workflow)(order.workflowId)
    yield
      order.namedValues(workflow)

  private[controller] def checkAddedOrChangedItems(itemKeys: Iterable[InventoryItemKey]): Checked[Unit] =
    itemKeys
      .flatMap: itemKey =>
        keyToItem.checked(itemKey)
          .flatTraverse(_
            .referencedItemPaths
            .map(path => referencedItemExists(path) !!
              MissingReferencedItemProblem(itemKey, referencedItemKey = path))
            .toVector)
      .combineProblems
      .map(_.combineAll)

  private def referencedItemExists(path: InventoryItemPath) =
    pathToItem.contains(path) ||
      (path match
        case id: SubagentBundleId =>
          // A SubagentId may be given instead of a SubagentBundleId.
          // SubagentKeeper handles this.
          pathToItem.contains(id.toSubagentId)
        case _ => false)

  private[controller] def checkRemovedVersionedItems(deletedPaths: Iterable[VersionedItemPath])
  : Checked[Unit] =
    deletedPaths.view
      .map(checkVersionedItemIsDeletable)
      .combineProblems
      .rightAs(())

  private def checkVersionedItemIsDeletable(path: VersionedItemPath): Checked[Unit] =
    referencingItemKeys(path)
      .toVector.sorted
      .map:
        itemIsStillReferencedProblem(path, _)
      .reduceLeftOption(Problem.combine)
      .toLeft(())

  private[controller] def checkDeletedSimpleItems(deletedPaths: Set[SimpleItemPath])
  : Checked[Unit] =
    deletedPaths.view
      .map(checkSimpleItemIsDeletable(_, deletedPaths))
      .combineProblems
      .rightAs(())

  private def checkSimpleItemIsDeletable(path: SimpleItemPath, otherDeleted: Set[SimpleItemPath])
  : Checked[Unit] =
    referencingItemKeys(path)
      .filter:
        case path: SimpleItemPath => !otherDeleted.contains(path)
        case _ => true
      .toVector.sorted
      .map: itemKey =>
        itemIsStillReferencedProblem(path, itemKey)
      .reduceLeftOption(Problem.combine)
      .toLeft(())

  private def itemIsStillReferencedProblem(
    itemPath: InventoryItemPath,
    referencingItemKey: InventoryItemKey)
  : ItemIsStillReferencedProblem =
    val elementsString = referencingItemKey match
      case WorkflowId.as(workflowId) =>
        val orderIds = workflowToOrders.workflowIdToOrders.get(workflowId).toVector.flatten
        orderIds.length match
          case 0 => ""
          case 1 => s" with ${orderIds.head}"
          case n => s" with $n Orders"
      case orderWatchPath: OrderWatchPath =>
        val externalNames = keyTo(OrderWatchState).get(orderWatchPath).toVector
          .flatMap(_.externalToState.keys)
        externalNames.length match
          case 0 => ""
          case 1 => s" with ${externalNames.head}"
          case n => s" with $n ExternalOrderNames"
      case _ => ""

    val agentPathsString =
      val agentPaths = agentAttachments.itemToDelegateToAttachedState.get(referencingItemKey)
        .toVector.flatMap(_.keys)
      agentPaths.nonEmpty ?? s", attached to ${agentPaths.mkString(", ")}"

    ItemIsStillReferencedProblem(itemPath, referencingItemKey,
      moreInfo = elementsString + agentPathsString)

  private[controller] def detach(itemKey: InventoryItemKey): View[ItemDetachable] =
    itemToAgentToAttachedState
      .getOrElse(itemKey, Map.empty)
      .view
      .flatMap:
        (agentPath, notDetached) => toDetachEvent(itemKey, agentPath, notDetached)

  private def toDetachEvent(itemKey: InventoryItemKey, agentPath: AgentPath, notDetached: NotDetached)
  : Option[ItemDetachable] =
    notDetached match
      case Attached(_) => Some(ItemDetachable(itemKey, agentPath))
      case _ => None

  def itemToAgentToAttachedState: Map[InventoryItemKey, Map[AgentPath, NotDetached]] =
    agentAttachments.itemToDelegateToAttachedState

  private[controller] def isReferenced(path: InventoryItemPath): Boolean =
    pathToReferencingItemKeys contains path

  private def referencingItemKeys(path: InventoryItemPath): View[InventoryItemKey] =
    pathToReferencingItemKeys.get(path).view.flatten

  @slow
  private[controller] lazy val pathToReferencingItemKeys: Map[InventoryItemPath, Seq[InventoryItemKey]] =
    items
      .flatMap(item => item.referencedItemPaths.map(item.key -> _))
      .groupMap(_._2)(_._1)
      .view
      .mapValues(_.toVector)
      .toMap
      .tap(o => logger.trace:
        s"pathToReferencingItemKeys: ${items.size} items => pathToReferencingItemKeys size=${o.size}")

  private[controller] def isObsoleteItem(itemId: VersionedItemId_) =
    !repo.isCurrentItem(itemId) && !isInUse(itemId)

  private[controller] def isInUse(itemId: VersionedItemId_) =
    itemId match
      case WorkflowId.as(workflowId) => isWorkflowUsedByOrders(workflowId)
      case _ => true

  @slow
  private[controller] lazy val isWorkflowUsedByOrders: Set[WorkflowId] =
    idToOrder.valuesIterator.map(_.workflowId).toSet
      .tap(o => logger.trace(s"${idToOrder.size} orders => isWorkflowUsedByOrders size=${o.size}"))

  def agentToUris(agentPath: AgentPath): Nel[Uri] =
    Nel.fromListUnsafe:
      for
        agentRef <- keyToItem(AgentRef).get(agentPath).toList
        director <- agentRef.directors
        subagent <- keyToItem(SubagentItem).get(director).toList
      yield
        subagent.uri

  def itemToAttachedState(itemKey: InventoryItemKey, itemRevision: Option[ItemRevision], agentPath: AgentPath)
  : ItemAttachedState =
    itemToAgentToAttachedState
      .get(itemKey)
      .flatMap(_.get(agentPath))
      .map:
        case a @ (Attachable | Attached(`itemRevision`) | Detachable) => a
        case Attached(_) => Detached
      .getOrElse(Detached)

  lazy val keyToItem: MapView[InventoryItemKey, InventoryItem] =
    new MapView[InventoryItemKey, InventoryItem]:
      def get(itemKey: InventoryItemKey) =
        itemKey match
          case id: VersionedItemId_ => repo.anyIdToItem(id).toOption
          case key: VersionedControlId_ => keyToUnsignedItemState_.get(key).map(_.item)
          case path: SimpleItemPath => pathToItem.get(path)

      def iterator = items.map(o => o.key -> o).iterator

      override def values = items

  def keyToUnsignedItemState: MapView[UnsignedItemKey, UnsignedItemState] =
    keyToUnsignedItemState_.view

  private lazy val pathToItem: MapView[InventoryItemPath, InventoryItem] =
    new MapView[InventoryItemPath, InventoryItem]:
      def get(path: InventoryItemPath): Option[InventoryItem] =
        path match
          case path: UnsignedSimpleItemPath => keyToUnsignedItemState_.get(path).map(_.item)
          case path: SignableSimpleItemPath => pathToSignedSimpleItem.get(path).map(_.value)
          case path: VersionedItemPath => repo.pathToVersionedItem(path)

      def iterator: Iterator[(InventoryItemPath, InventoryItem)] =
        simpleItems.view.map(item => item.key -> item).iterator ++
          repo.currentItems.iterator.map(o => o.path -> o)

  def items: View[InventoryItem] =
    keyToUnsignedItemState_.values.view.map(_.item) ++
      signableItems

  def simpleItems: View[SimpleItem] =
    unsignedSimpleItems ++ signableSimpleItems

  private def unsignedSimpleItems: View[UnsignedSimpleItem] =
    keyToUnsignedItemState_.values.view
      .collect { case o: UnsignedSimpleItemState => o }
      .map(_.item)

  lazy val idToWorkflow: PartialFunction[WorkflowId, Workflow] =
    new PartialFunction[WorkflowId, Workflow]:
      def isDefinedAt(workflowId: WorkflowId) =
        repo.idToSigned(Workflow)(workflowId).isRight

      def apply(workflowId: WorkflowId): Workflow =
        repo.idToSigned(Workflow)(workflowId).orThrow.value

      override def applyOrElse[K <: WorkflowId, V >: Workflow](workflowId: K, default: K => V): V =
        repo.idToSigned(Workflow)(workflowId)
          .fold(_ => default(workflowId), _.value)

  def workflowPathToId(workflowPath: WorkflowPath): Checked[WorkflowId] =
    repo.pathToId(workflowPath)
      .toRight(UnknownKeyProblem("WorkflowPath", workflowPath.string))

  def pathToJobResource: MapView[JobResourcePath, JobResource] = keyTo(JobResource)

  lazy val keyToSignedItem: MapView[SignableItemKey, Signed[SignableItem]] =
    new MapView[SignableItemKey, Signed[SignableItem]]:
      def get(itemKey: SignableItemKey): Option[Signed[SignableItem]] =
        itemKey match
          case id: VersionedItemId_ => repo.anyIdToSigned(id).toOption
          case path: SignableSimpleItemPath => pathToSignedSimpleItem.get(path)

      def iterator: Iterator[(SignableItemKey, Signed[SignableItem])] =
        signedItems.iterator.map(o => o.value.key -> o)

  def signableSimpleItems: View[SignableSimpleItem] =
    pathToSignedSimpleItem.values.view.map(_.value)

  private def signableItems: View[SignableItem] =
    signedItems.view.map(_.value)

  private def signedItems: View[Signed[SignableItem]] =
    pathToSignedSimpleItem.values.view ++
      repo.signedItems

  def orders: Iterable[Order[Order.State]] =
    idToOrder.values

  def finish: Checked[ControllerState] =
    PlanSchemaState
      .recoverPlanSchemaStatesFromOrders(orders, keyTo(PlanSchemaState).checked)
      .map: planSchemaStates =>
        copy(
          workflowToOrders = calculateWorkflowToOrders,
          keyToUnsignedItemState_ = keyToUnsignedItemState_ ++
            planSchemaStates.toKeyedMap(_.id))

  private def calculateWorkflowToOrders: WorkflowToOrders =
    ControllerState.WorkflowToOrders(idToOrder
      .values.view
      .map(o => o.workflowId -> o.id)
      .toVector
      .groupMap[WorkflowId, OrderId](_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap)

  /** See also toStringStream! */
  override def toString = s"ControllerState(${EventId.toString(eventId)} ${
    idToOrder.size} orders, Repo(${repo.currentVersionSize} objects, ...)"


object ControllerState
extends
  EventDrivenStateView.Companion[ControllerState],
  ClusterableState.Companion[ControllerState],
  ItemContainer.Companion[ControllerState]:

  private val logger = Logger[this.type]

  // Required because controllerMetaState.controllerId must be initialized first.
  // Controller will call afterAggregateInitialisation.
  override val callExpliclitlyAfterAggregateInitialisation = true

  val Undefined: ControllerState = ControllerState(
    EventId.BeforeFirst,
    SnapshotableState.Standards.empty,
    ControllerMetaState.Undefined,
    Map.empty,
    Repo.empty,
    Map.empty,
    ClientAttachments.empty,
    Set.empty,
    Map.empty,
    EngineStateStatistics.empty,
    WorkflowToOrders(Map.empty))

  val empty: ControllerState =
    Undefined.copy(
      keyToUnsignedItemState_ = Undefined.keyToUnsignedItemState_
        .updated(PlanSchemaId.Global, PlanSchemaState.initialGlobal))

  def newRecoverer() = new ControllerStateRecoverer

  protected val inventoryItems = Vector[InventoryItem.Companion_](
    AgentRef, SubagentItem, SubagentBundle, Lock,
    GlobalBoard, PlanSchema, PlannableBoard,
    Calendar, FileWatch, JobResource,
    Workflow, WorkflowPathControl, WorkflowControl)

  lazy val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec[Any](
      Subtype[JournalHeader],
      Subtype[SnapshotMeta],
      Subtype[JournalState],
      Subtype[ClusterStateSnapshot],
      Subtype[ControllerMetaState],
      Subtype[AgentRefState],
      Subtype[SubagentItemState](Nil, aliases = Seq("SubagentRefState")),
      SubagentBundle.subtype,
      Subtype[LockState],
      GlobalBoard.subtype,
      PlannableBoard.subtype,
      PlanSchema.subtype,
      PlanSchemaState.Snapshot.subtype,
      Plan.subtype,
      Calendar.subtype,
      Subtype[Notice],
      NoticePlace.Snapshot.subtype,
      BoardState.NoticeConsumptionSnapshot.subtype,
      Subtype[VersionedEvent],  // These events describe complete objects
      Subtype[InventoryItemEvent],  // For Repo and SignedItemAdded
      Subtype[OrderWatchState.Snapshot],
      Subtype[Order[Order.State]],
      WorkflowPathControl.subtype,
      WorkflowControl.subtype,
      EventCount.subtype)

  implicit lazy val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec/*.named("ControllerState.keyedEventJsonCodec"*/(
      KeyedSubtype[JournalEvent],
      KeyedSubtype[InventoryItemEvent],
      KeyedSubtype[VersionedEvent],
      KeyedSubtype[ControllerEvent],
      KeyedSubtype[ClusterEvent],
      KeyedSubtype[AgentRefStateEvent],
      KeyedSubtype[SubagentItemStateEvent],
      KeyedSubtype[OrderWatchEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype[NoticeEvent],
      KeyedSubtype[PlanSchemaEvent],
      KeyedSubtype[PlanEvent],
      KeyedSubtype.singleEvent[ServerMeteringEvent])

  object implicits:
    implicit val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
      ControllerState.snapshotObjectJsonCodec

  private def update(
    s: ControllerState,
    updateOrders: Seq[Order[Order.State]] = Nil,
    removeOrders: Seq[OrderId] = Nil,
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState] = Nil,
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath] = Nil)
  : Checked[ControllerState] =
    for
      s <- this.updateOrders(s, updateOrders ++ externalVanishedOrders)
      s <- externalVanishedOrders.foldEithers(s):
        _.ow.onOrderExternalVanished(_)
      s <- removeOrders_(s, removeOrders)
      s <- Right(s.copy(
        keyToUnsignedItemState_ = s.keyToUnsignedItemState_
          -- removeUnsignedSimpleItems
          ++ addItemStates.view.map(o => o.path -> o)))
    yield
      s

  private def addOrders(s: ControllerState, orders: Seq[Order[Order.State]], allowClosedPlan: Boolean)
  : Checked[ControllerState] =
    if orders.isEmpty then
      Right(s)
    else
      s.checkOrdersDoNotExist(orders.view.map(_.id)).flatMap: _ =>
        for
          s <- orders.foldEithers(s)(addOrUpdateOrder_)
          updatedPlanSchemaStates <- PlanSchemaState.addOrderIds(
            orders,
            s.keyTo(PlanSchemaState).checked,
            allowClosedPlan = allowClosedPlan)
        yield
          s.copy(
            keyToUnsignedItemState_ = s.keyToUnsignedItemState_
              ++ updatedPlanSchemaStates.map(o => o.id -> o))

  private def updateOrders(s: ControllerState, orders: Seq[Order[Order.State]])
  : Checked[ControllerState] =
    // TODO Differentiate between add and update?
    if orders.isEmpty then
      Right(s)
    else
      s.checkOrdersExist(orders.view.map(_.id)).flatMap: _ =>
        orders.foldEithers(s)(addOrUpdateOrder_)

  private def addOrUpdateOrder_(s: ControllerState, order: Order[Order.State])
  : Checked[ControllerState] =
    if s.idToOrder contains order.id then
      Right(s.copy(
        idToOrder = s.idToOrder.updated(order.id, order)))
    else
      s.ow.onOrderAdded(order).map: s =>
        s.copy(
          idToOrder = s.idToOrder.updated(order.id, order),
          workflowToOrders = s.workflowToOrders.addOrder(order))

  private def removeOrders_(s: ControllerState, orderIds: Seq[OrderId]): Checked[ControllerState] =
    if orderIds.isEmpty then
      Right(s)
    else
      val removedOrders = orderIds.flatMap(s.idToOrder.get)
      orderIds.foldEithers(s): (s, orderId) =>
        s.idToOrder.get(orderId).fold(Checked(s)): order =>
          order.externalOrder.fold(Checked(s)): ext =>
            s.ow.onOrderDeleted(ext.externalOrderKey, orderId)
          .map: s =>
            s.copy(workflowToOrders = s.workflowToOrders.removeOrder(order))
      .map: s =>
        s.copy(idToOrder = s.idToOrder -- orderIds)
      .flatMap: s =>
        PlanSchemaState
          .removeOrderIds(removedOrders, s.keyTo(PlanSchemaState).checked)
          .map: updatedPlanSchemaStates =>
            s.copy(
              idToOrder = s.idToOrder -- orderIds,
              keyToUnsignedItemState_ = s.keyToUnsignedItemState_ ++
                updatedPlanSchemaStates.toKeyedMap(_.id))


  final case class WorkflowToOrders(workflowIdToOrders: Map[WorkflowId, Set[OrderId]]):
    def transferOrder(order: Order[Order.State], to: WorkflowId): WorkflowToOrders =
      removeOrder(order).addOrder(order.id, to)

    def addOrder(order: Order[Order.State]): WorkflowToOrders =
      addOrder(order.id, order.workflowId)

    private def addOrder(orderId: OrderId, workflowId: WorkflowId): WorkflowToOrders =
      copy(workflowIdToOrders.updated(
        workflowId,
        workflowIdToOrders.getOrElse(workflowId, Set.empty) + orderId))

    def removeOrder(order: Order[Order.State]): WorkflowToOrders =
      val orderIds = workflowIdToOrders(order.workflowId) - order.id
      if orderIds.isEmpty then
        copy(workflowIdToOrders - order.workflowId)
      else
        copy(workflowIdToOrders.updated(order.workflowId, orderIds))

    def isLastOrder(order: Order[Order.State]): Boolean =
      workflowIdToOrders.get(order.workflowId).contains(Set(order.id))

    def isEmpty: Boolean =
      workflowIdToOrders.isEmpty

    inline def nonEmpty: Boolean =
      !isEmpty

    def toStringStream: fs2.Stream[fs2.Pure, String] =
      Stream.emit(s"WorkflowToOrders:\n") ++
        Stream.iterable(workflowIdToOrders.toVector.sortBy(_._1)).flatMap: (k, v) =>
          Stream.emit(s"  $k\n") ++
            Stream.iterable(v.toVector.sorted.map(_.toString)).map(o => s"    $o")

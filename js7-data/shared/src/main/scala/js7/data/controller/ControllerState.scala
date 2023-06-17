package js7.data.controller

import cats.syntax.foldable.*
import cats.syntax.traverse.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.problem.Checked.RichCheckedIterable
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.data.Problems.{ItemIsStillReferencedProblem, MissingReferencedItemProblem}
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRefStateEvent}
import js7.data.board.BoardEvent.{NoticeDeleted, NoticePosted}
import js7.data.board.{Board, BoardEvent, BoardPath, BoardState, Notice, NoticePlace}
import js7.data.calendar.{Calendar, CalendarPath, CalendarState}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.ControllerTestEvent
import js7.data.controller.ControllerState.{DummyClusterNodeName, WorkflowToOrders, logger}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{Event, EventId, ItemContainer, JournalEvent, JournalHeader, JournalState, KeyedEvent, KeyedEventTypedJsonCodec, SignedItemContainer, SnapshotMeta, SnapshotableState}
import js7.data.item.BasicItemEvent.{ItemAttachedStateEvent, ItemDeleted, ItemDeletionMarked, ItemDetachable}
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable, Detached, NotDetached}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedItemEvent.{UnsignedItemAdded, UnsignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, ClientAttachments, InventoryItem, InventoryItemEvent, InventoryItemKey, InventoryItemPath, ItemAttachedState, ItemRevision, Repo, SignableItem, SignableItemKey, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, SimpleItem, SimpleItemPath, UnsignedItemEvent, UnsignedItemKey, UnsignedItemState, UnsignedSimpleItem, UnsignedSimpleItemEvent, UnsignedSimpleItemPath, UnsignedSimpleItemState, VersionedControl, VersionedControlId_, VersionedEvent, VersionedItemId_, VersionedItemPath}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.node.{NodeId, NodeName}
import js7.data.order.OrderEvent.{OrderNoticesExpected, OrderTransferred}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatch, OrderWatchEvent, OrderWatchPath, OrderWatchState, OrderWatchStateHandler}
import js7.data.state.EventDrivenStateView
import js7.data.subagent.SubagentItemStateEvent.SubagentShutdown
import js7.data.subagent.{SubagentId, SubagentItem, SubagentItemState, SubagentItemStateEvent, SubagentSelection, SubagentSelectionId, SubagentSelectionState}
import js7.data.value.Value
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowControlId, WorkflowId, WorkflowPath, WorkflowPathControl, WorkflowPathControlPath}
import monix.reactive.Observable
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
  workflowToOrders: WorkflowToOrders = WorkflowToOrders(Map.empty))
extends SignedItemContainer
with EventDrivenStateView[ControllerState, Event]
with OrderWatchStateHandler[ControllerState]
with SnapshotableState[ControllerState]
{
  def isAgent = false

  def controllerId = controllerMetaState.controllerId

  def companion = ControllerState

  def clusterNodeIdToName(nodeId: NodeId) =
    Right(DummyClusterNodeName)

  def clusterNodeToUserId(nodeId: NodeId) =
    Right(controllerId.toUserId)

  def estimatedSnapshotSize: Int =
    1 +
    standards.snapshotSize +
    controllerMetaState.isDefined.toInt +
    repo.estimatedEventCount +
    keyToUnsignedItemState_.size +
    keyTo(OrderWatchState).values.view.map(_.estimatedSnapshotSize - 1).sum +
    keyTo(BoardState).values.view.map(_.noticeCount).sum +
    pathToSignedSimpleItem.size +
    agentAttachments.estimatedSnapshotSize +
    deletionMarkedItems.size +
    idToOrder.size

  def toSnapshotObservable: Observable[Any] =
    Observable(
      Observable.pure(SnapshotEventId(eventId)),
      standards.toSnapshotObservable,
      Observable.fromIterable(controllerMetaState.isDefined ? controllerMetaState),
      Observable.fromIterable(keyTo(WorkflowPathControl).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(keyTo(WorkflowControl).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(keyTo(AgentRefState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(keyTo(SubagentItemState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(keyTo(SubagentSelectionState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(keyTo(LockState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(keyTo(BoardState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(keyTo(CalendarState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(keyTo(OrderWatchState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(pathToSignedSimpleItem.values).map(SignedItemAdded(_)),
      Observable.fromIterable(repo.toEvents),
      agentAttachments.toSnapshotObservable,
      Observable.fromIterable(deletionMarkedItems.map(ItemDeletionMarked(_))),
      Observable.fromIterable(idToOrder.values)
    ).flatten

  def withEventId(eventId: EventId) =
    copy(eventId = eventId)

  def withStandards(standards: SnapshotableState.Standards) =
    copy(standards = standards)

  def applyEvent(keyedEvent: KeyedEvent[Event]) = keyedEvent match {
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
      event match {
        case event: UnsignedSimpleItemEvent =>
          event match {
            case UnsignedSimpleItemAdded(item) =>
              item match {
                case addedAgentRef: AgentRef =>
                  addedAgentRef
                    .convertFromV2_1
                    .flatMap { case (agentRef, maybeSubagentItem) =>
                      for {
                        pathToItemState <-
                          keyToUnsignedItemState_.insert(agentRef.path, AgentRefState(agentRef))
                        pathToItemState <-
                          maybeSubagentItem match {
                            case None => Right(pathToItemState)
                            case Some(subagentItem) => pathToItemState
                              .insert(subagentItem.id, SubagentItemState.initial(subagentItem))
                          }
                      } yield copy(
                        keyToUnsignedItemState_ = pathToItemState)
                    }

                case orderWatch: OrderWatch =>
                  ow.addOrderWatch(orderWatch.toInitialItemState)

                case item: UnsignedSimpleItem =>
                  for (o <- keyToUnsignedItemState_.insert(item.path, item.toInitialItemState)) yield
                    copy(keyToUnsignedItemState_ = o)
              }

            case UnsignedSimpleItemChanged(item) =>
              item match {
                case changedAgentRef: AgentRef =>
                  changedAgentRef
                    .convertFromV2_1
                    .flatMap { case (agentRef, maybeSubagentItem) =>
                      def checkIsExtending(a: AgentRef, b: AgentRef) =
                        (a.directors == b.directors
                          || a.directors.length == 1 && b.directors.length == 2
                          && a.directors(0) == b.directors(0)
                        ) !! Problem.pure("Agent Directors cannot not be changed")

                      for {
                        agentRefState <- keyTo(AgentRefState).checked(agentRef.path)
                        _ <- checkIsExtending(agentRefState.agentRef, agentRef)
                        updatedAgentRef <- agentRefState.updateItem(agentRef)
                      } yield
                        copy(
                          keyToUnsignedItemState_ = keyToUnsignedItemState_
                            .updated(agentRef.path, updatedAgentRef)
                            .pipeMaybe(maybeSubagentItem)((pathToItemState, changedSubagentItem) =>
                              // COMPATIBLE with v2.2.2
                              keyTo(SubagentItemState)
                                .get(changedSubagentItem.id)
                                .fold(pathToItemState)(subagentItemState =>
                                  pathToItemState.updated(changedSubagentItem.id,
                                    subagentItemState.copy(
                                      subagentItem = subagentItemState.item
                                        .updateUri(changedSubagentItem.uri))))))
                    }

                case orderWatch: OrderWatch =>
                  ow.changeOrderWatch(orderWatch)

                case item: UnsignedSimpleItem =>
                  for {
                    itemState <- keyToUnsignedItemState_.checked(item.path)
                    updated <- itemState.updateItem(item.asInstanceOf[itemState.companion.Item])
                  } yield
                    copy(
                      keyToUnsignedItemState_ =
                        keyToUnsignedItemState_.updated(item.path, updated))
              }
          }

        case event: SignedItemEvent =>
          event match {
            case SignedItemAdded(Signed(item, signedString)) =>
              item match {
                case jobResource: JobResource =>
                  for (o <- pathToSignedSimpleItem.insert(jobResource.path -> Signed(jobResource, signedString))) yield
                    copy(pathToSignedSimpleItem = o)
              }

            case SignedItemChanged(Signed(item, signedString)) =>
              item match {
                case jobResource: JobResource =>
                  Right(copy(
                    pathToSignedSimpleItem = pathToSignedSimpleItem + (jobResource.path -> Signed(jobResource, signedString))))
              }
          }

        case event: UnsignedItemEvent =>
          event match {
            case UnsignedItemAdded(item: VersionedControl) =>
              keyToUnsignedItemState_
                .insert(item.key, item.toInitialItemState)
                .map(o => copy(keyToUnsignedItemState_ = o))

            case UnsignedItemChanged(item: VersionedControl) =>
              Right(copy(
                keyToUnsignedItemState_ = keyToUnsignedItemState_.updated(item.key, item.toInitialItemState)))
          }

        case event: BasicItemEvent.ForClient =>
          event match {
            case event: ItemAttachedStateEvent =>
              for (o <- agentAttachments.applyEvent(event)) yield
                copy(agentAttachments = o)

            case ItemDeletionMarked(itemKey) =>
              Right(copy(
                deletionMarkedItems = deletionMarkedItems + itemKey))

            case event @ ItemDeleted(itemKey) =>
              val updated = copy(
                deletionMarkedItems = deletionMarkedItems - itemKey,
                agentAttachments = agentAttachments.applyItemDeleted(event))

              itemKey match {
                case WorkflowId.as(workflowId) =>
                  for (repo <- repo.deleteItem(workflowId)) yield
                    updated.copy(
                      repo = repo)

                case path: OrderWatchPath =>
                  updated.ow.removeOrderWatch(path)

                case jobResourcePath: JobResourcePath =>
                  Right(updated.copy(
                    pathToSignedSimpleItem = pathToSignedSimpleItem - jobResourcePath))

                case key: UnsignedItemKey =>
                  key match {
                    case _: AgentPath | _: SubagentId | _: SubagentSelectionId |
                         _: LockPath | _: BoardPath | _: CalendarPath |
                         _: WorkflowPathControlPath | WorkflowControlId.as(_) =>
                      Right(updated.copy(
                        keyToUnsignedItemState_ = keyToUnsignedItemState_ - key))
                    case _ =>
                      Left(Problem(s"A '${ itemKey.companion.itemTypeName }' is not deletable"))
                  }

                case _ =>
                  Left(Problem(s"A '${itemKey.companion.itemTypeName}' is not deletable"))
              }
          }
      }

    case KeyedEvent(_: NoKey, event: VersionedEvent) =>
      for (o <- repo.applyEvent(event)) yield
        copy(repo = o)

    case KeyedEvent(agentPath: AgentPath, event: AgentRefStateEvent) =>
      for {
        agentRefState <- keyTo(AgentRefState).checked(agentPath)
        agentRefState <- agentRefState.applyEvent(event)
      } yield copy(
        keyToUnsignedItemState_ = keyToUnsignedItemState_ + (agentPath -> agentRefState))

    case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
      event match {
        case event: OrderTransferred =>
          for (updated <- applyOrderEvent(orderId, event)) yield
            updated
              .copy(workflowToOrders = workflowToOrders
                .moveOrder(idToOrder(orderId), updated.idToOrder(orderId).workflowId))

        case _ =>
          applyOrderEvent(orderId, event)
      }

    case KeyedEvent(boardPath: BoardPath, NoticePosted(notice)) =>
      for {
        boardState <- keyTo(BoardState).checked(boardPath)
        o <- boardState.addNotice(notice.toNotice(boardPath))
      } yield copy(
        keyToUnsignedItemState_ = keyToUnsignedItemState_.updated(o.path, o))

    case KeyedEvent(boardPath: BoardPath, NoticeDeleted(noticeId)) =>
      for {
        boardState <- keyTo(BoardState).checked(boardPath)
        o <- boardState.removeNotice(noticeId)
      } yield copy(
        keyToUnsignedItemState_ = keyToUnsignedItemState_.updated(o.path, o))

    case KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent) =>
      ow.onOrderWatchEvent(orderWatchPath <-: event)

    case KeyedEvent(subagentId: SubagentId, event: SubagentItemStateEvent) =>
      event match {
        case SubagentShutdown if !keyToUnsignedItemState_.contains(subagentId) =>
          // May arrive when SubagentItem has been deleted
          Right(this)

        case _ =>
          for {
            o <- keyTo(SubagentItemState).checked(subagentId)
            o <- o.applyEvent(event)
          } yield copy(
            keyToUnsignedItemState_ = keyToUnsignedItemState_.updated(subagentId, o))
      }

    case KeyedEvent(_, ControllerTestEvent) =>
      Right(this)

    case _ => applyStandardEvent(keyedEvent)
  }

  /** The Agents for each WorkflowPathControl which have not attached the current itemRevision. */
  def workflowPathControlToIgnorantAgents: MapView[WorkflowPathControlPath, Set[AgentPath]] =
    new MapView[WorkflowPathControlPath, Set[AgentPath]] {
      private val pathToWorkflowPathControl = keyTo(WorkflowPathControl)

      def get(key: WorkflowPathControlPath): Option[Set[AgentPath]] =
        pathToWorkflowPathControl.get(key).map(attachableToAgents)

      def iterator: Iterator[(WorkflowPathControlPath, Set[AgentPath])] =
        pathToWorkflowPathControl.iterator
          .flatMap { case (k, v) =>
            val agents = attachableToAgents(v)
            agents.nonEmpty ? (k -> agents)
          }

      override def values: Iterable[Set[AgentPath]] =
        pathToWorkflowPathControl.values.view.map(attachableToAgents)

      private def attachableToAgents(workflowPathControl: WorkflowPathControl): Set[AgentPath] =
        itemToAgentToAttachedState
          .get(workflowPathControl.path)
          .view
          .flatMap(_.collect {
            case (agentPath, Attachable) => agentPath
          })
          .toSet
    }

  /** The Agents for each InventoryItemKey which have not attached the current Item. */
  def itemToIgnorantAgents[I <: InventoryItem](I: InventoryItem.Companion[I])
  : MapView[I.Key, Set[AgentPath]] =
    filteredItemToIgnorantAgents[I.Key](_.companion eq I.Key)

  def anyItemToIgnorantAgents: MapView[InventoryItemKey, Set[AgentPath]] =
    filteredItemToIgnorantAgents[InventoryItemKey](_ => true)

  /** The Agents for each InventoryItemKey which have not attached the current Item. */
  private def filteredItemToIgnorantAgents[K <: InventoryItemKey](filter_ : InventoryItemKey => Boolean)
  : MapView[K, Set[AgentPath]] =
    new MapView[K, Set[AgentPath]] {
      def get(key: K): Option[Set[AgentPath]] =
        itemToAgentToAttachedState.get(key).flatMap(toAgents)

      def iterator: Iterator[(K, Set[AgentPath])] =
        itemToAgentToAttachedState.iterator
          .flatMap {
            case (k: K @unchecked, v) if filter_(k) =>
              toAgents(v).map(k -> _)
            case _ => None
          }

      private def toAgents(agentToAttachedState: Map[AgentPath, ItemAttachedState])
      : Option[Set[AgentPath]] = {
        val agents = agentToAttachedState
          .collect { case (agentPath, Attachable) => agentPath }
          .toSet
        agents.nonEmpty ? agents
      }
    }

  def orderToAvailableNotices(orderId: OrderId): Seq[Notice] = {
    val pathToBoardState = keyTo(BoardState)
    orderToExpectedNotices(orderId)
      .flatMap(expected =>
        pathToBoardState
          .get(expected.boardPath)
          .flatMap(_
            .idToNotice.get(expected.noticeId)
            .flatMap(_.notice)))
  }

  def orderToStillExpectedNotices(orderId: OrderId): Seq[OrderNoticesExpected.Expected] = {
    val pathToBoardState = keyTo(BoardState)
    orderToExpectedNotices(orderId)
      .filter(expected =>
        pathToBoardState
          .get(expected.boardPath)
          .forall(boardState => !boardState
            .idToNotice.get(expected.noticeId)
            .exists(_.notice.isDefined)))
  }

  private def orderToExpectedNotices(orderId: OrderId): Seq[OrderNoticesExpected.Expected] =
    idToOrder.get(orderId)
      .flatMap(_.ifState[Order.ExpectingNotices])
      .toVector
      .flatMap(_.state.expected)

  protected def pathToOrderWatchState = keyTo(OrderWatchState)

  protected def updateOrderWatchStates(
    orderWatchStates: Iterable[OrderWatchState],
    remove: Iterable[OrderWatchPath]
  ) = update(addItemStates = orderWatchStates, removeItemStates = remove)

  protected def update(
    orders: Iterable[Order[Order.State]],
    removeOrders: Iterable[OrderId],
    addItemStates: Iterable[UnsignedSimpleItemState],
    removeItemStates: Iterable[UnsignedSimpleItemPath])
  : Checked[ControllerState] = {
    var result: Checked[ControllerState] = Right(this)
    for (order <- orders; controllerState <- result) {
      result = controllerState.addOrUpdateOrder(order)
    }
    for (orderId <- removeOrders; order <- idToOrder.get(orderId); controllerState <- result) {
      result = controllerState.deleteOrder(order)
    }
    for (controllerState <- result) {
      result = Right(controllerState.copy(
        keyToUnsignedItemState_ = keyToUnsignedItemState_
          -- removeItemStates
          ++ addItemStates.view.map(o => o.path -> o)))
    }
    result
  }

  private def addOrUpdateOrder(order: Order[Order.State]): Checked[ControllerState] =
    if (idToOrder contains order.id)
      Right(copy(
        idToOrder = idToOrder.updated(order.id, order)))
    else
      continueAddOrder(order)

  override protected def addOrder(order: Order[Order.State]): Checked[ControllerState] =
    for {
      _ <- idToOrder.checkNoDuplicate(order.id)
      updated <- continueAddOrder(order)
    } yield updated

  private def continueAddOrder(order: Order[Order.State]): Checked[ControllerState] =
    for (updated <- ow.onOrderAdded(order)) yield
      updated.copy(
        idToOrder = idToOrder.updated(order.id, order),
        workflowToOrders = workflowToOrders.addOrder(order))

  override protected def deleteOrder(order: Order[Order.State]): Checked[ControllerState] =
    for (updated <- order.externalOrderKey.fold_(Right(this), ow.onOrderDeleted(_, order.id))) yield
      updated.copy(
        idToOrder = idToOrder - order.id,
        workflowToOrders = workflowToOrders.removeOrder(order))

  /** The named values as seen at the current workflow position. */
  def orderNamedValues(orderId: OrderId): Checked[MapView[String, Value]] =
    for {
      order <- idToOrder.checked(orderId)
      workflow <- repo.idTo(Workflow)(order.workflowId)
    } yield order.namedValues(workflow)

  private[controller] def checkAddedOrChangedItems(itemKeys: Iterable[InventoryItemKey]): Checked[Unit] =
    itemKeys
      .flatMap(itemKey =>
        keyToItem.checked(itemKey)
          .flatTraverse(_
            .referencedItemPaths
            .map(path => referencedItemExists(path) !!
              MissingReferencedItemProblem(itemKey, referencedItemKey = path))
            .toVector))
      .combineProblems
      .map(_.combineAll)

  private def referencedItemExists(path: InventoryItemPath) =
    pathToItem.contains(path) ||
      (path match {
        case id: SubagentSelectionId =>
          // A SubagentId may be given instead of a SubagentSelectionId.
          // SubagentKeeper handles this.
          pathToItem.contains(id.toSubagentId)
        case _ => false
      })


  private[controller] def checkRemovedVersionedItems(deletedPaths: Iterable[VersionedItemPath])
  : Checked[Unit] =
    deletedPaths.view
      .map(checkVersionedItemIsDeletable)
      .combineProblems
      .rightAs(())

  private def checkVersionedItemIsDeletable(path: VersionedItemPath): Checked[Unit] =
    referencingItemKeys(path)
      .toVector
      .sortBy(_.path)
      .map(ItemIsStillReferencedProblem(path, _))
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
      .filter {
        case path: SimpleItemPath => !otherDeleted.contains(path)
        case _ => true
      }
      .toVector
      .sortBy(_.path)
      .map(ItemIsStillReferencedProblem(path, _))
      .reduceLeftOption(Problem.combine)
      .toLeft(())

  private[controller] def detach(itemKey: InventoryItemKey): View[ItemDetachable] =
    itemToAgentToAttachedState
      .getOrElse(itemKey, Map.empty)
      .view
      .flatMap {
        case (agentPath, notDetached) => toDetachEvent(itemKey, agentPath, notDetached)
      }

  private def toDetachEvent(itemKey: InventoryItemKey, agentPath: AgentPath, notDetached: NotDetached)
  : Option[ItemDetachable] =
    notDetached match {
      case Attached(_) => Some(ItemDetachable(itemKey, agentPath))
      case _ => None
    }

  def itemToAgentToAttachedState: Map[InventoryItemKey, Map[AgentPath, NotDetached]] =
    agentAttachments.itemToDelegateToAttachedState

  private[controller] def isReferenced(path: InventoryItemPath): Boolean =
    pathToReferencingItemKeys contains path

  private def referencingItemKeys(path: InventoryItemPath): View[InventoryItemKey] =
    pathToReferencingItemKeys.get(path).view.flatten

  // Slow ???
  private[controller] lazy val pathToReferencingItemKeys: Map[InventoryItemPath, Seq[InventoryItemKey]] =
    items
      .flatMap(item => item.referencedItemPaths.map(item.key -> _))
      .groupMap(_._2)(_._1)
      .view
      .mapValues(_.toVector)
      .toMap
      .tap(o => logger.trace(s"${items.size} items => pathToReferencingItemKeys size=${o.size}"))

  private[controller] def isObsoleteItem(itemId: VersionedItemId_) =
    !repo.isCurrentItem(itemId) && !isInUse(itemId)

  private[controller] def isInUse(itemId: VersionedItemId_) =
    itemId match {
      case WorkflowId.as(workflowId) => isWorkflowUsedByOrders(workflowId)
      case _ => true
    }

  // Slow ???
  private[controller] lazy val isWorkflowUsedByOrders: Set[WorkflowId] =
    idToOrder.valuesIterator.map(_.workflowId).toSet
      .tap(o => logger.trace(s"${idToOrder.size} orders => isWorkflowUsedByOrders size=${o.size}"))

  def agentToUris(agentPath: AgentPath): Nel[Uri] =
    Nel.fromListUnsafe(
      for {
        agentRef <- keyToItem(AgentRef).get(agentPath).toList
        director <- agentRef.directors
        subagent <- keyToItem(SubagentItem).get(director).toList
      } yield subagent.uri)

  def itemToAttachedState(itemKey: InventoryItemKey, itemRevision: Option[ItemRevision], agentPath: AgentPath)
  : ItemAttachedState =
    itemToAgentToAttachedState
      .get(itemKey)
      .flatMap(_.get(agentPath))
      .map {
        case a @ (Attachable | Attached(`itemRevision`) | Detachable) => a
        case Attached(_) => Detached
      }
      .getOrElse(Detached)

  lazy val keyToItem: MapView[InventoryItemKey, InventoryItem] =
    new MapView[InventoryItemKey, InventoryItem] {
      def get(itemKey: InventoryItemKey) =
        itemKey match {
          case id: VersionedItemId_ => repo.anyIdToItem(id).toOption
          case key: VersionedControlId_ => keyToUnsignedItemState_.get(key).map(_.item)
          case path: SimpleItemPath => pathToItem.get(path)
        }

      def iterator = items.map(o => o.key -> o).iterator

      override def values = items
    }

  def keyToUnsignedItemState: MapView[UnsignedItemKey, UnsignedItemState] =
    keyToUnsignedItemState_.view

  private lazy val pathToItem: MapView[InventoryItemPath, InventoryItem] =
    new MapView[InventoryItemPath, InventoryItem] {
      def get(path: InventoryItemPath): Option[InventoryItem] = {
        path match {
          case path: UnsignedSimpleItemPath => keyToUnsignedItemState_.get(path).map(_.item)
          case path: SignableSimpleItemPath => pathToSignedSimpleItem.get(path).map(_.value)
          case path: VersionedItemPath => repo.pathToVersionedItem(path).toOption
        }
      }

      def iterator: Iterator[(InventoryItemPath, InventoryItem)] =
        simpleItems.view.map(item => item.key -> item).iterator ++
          repo.currentItems.iterator.map(o => o.path -> o)
    }

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
    new PartialFunction[WorkflowId, Workflow] {
      def isDefinedAt(workflowId: WorkflowId) =
        repo.idToSigned(Workflow)(workflowId).isRight

      def apply(workflowId: WorkflowId): Workflow =
        repo.idToSigned(Workflow)(workflowId).orThrow.value

      override def applyOrElse[K <: WorkflowId, V >: Workflow](workflowId: K, default: K => V): V =
        repo.idToSigned(Workflow)(workflowId)
          .fold(_ => default(workflowId), _.value)
    }

  def workflowPathToId(workflowPath: WorkflowPath) =
    repo.pathToId(workflowPath)
      .toRight(UnknownKeyProblem("WorkflowPath", workflowPath.string))

  def pathToJobResource = keyTo(JobResource)

  lazy val keyToSignedItem: MapView[SignableItemKey, Signed[SignableItem]] =
    new MapView[SignableItemKey, Signed[SignableItem]] {
      def get(itemKey: SignableItemKey): Option[Signed[SignableItem]] =
        itemKey match {
          case id: VersionedItemId_ => repo.anyIdToSigned(id).toOption
          case path: SignableSimpleItemPath => pathToSignedSimpleItem.get(path)
        }

      def iterator: Iterator[(SignableItemKey, Signed[SignableItem])] =
        signedItems.iterator.map(o => o.value.key -> o)
    }

  def signableSimpleItems: View[SignableSimpleItem] =
    pathToSignedSimpleItem.values.view.map(_.value)

  private def signableItems: View[SignableItem] =
    signedItems.view.map(_.value)

  private def signedItems: View[Signed[SignableItem]] =
    pathToSignedSimpleItem.values.view ++
      repo.signedItems

  def orders = idToOrder.values

  def finish: ControllerState =
    copy(
      workflowToOrders = calculateWorkflowToOrders)

  private def calculateWorkflowToOrders: WorkflowToOrders =
    ControllerState.WorkflowToOrders(idToOrder
      .values.view
      .map(o => o.workflowId -> o.id)
      .toVector
      .groupMap[WorkflowId, OrderId](_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap)

  override def toString = s"ControllerState(${EventId.toString(eventId)} ${idToOrder.size} orders, " +
    s"Repo(${repo.currentVersionSize} objects, ...))"
}

object ControllerState
extends SnapshotableState.Companion[ControllerState]
with ItemContainer.Companion[ControllerState]
{
  private val logger = scribe.Logger[this.type]

  val Undefined = ControllerState(
    EventId.BeforeFirst,
    SnapshotableState.Standards.empty,
    ControllerMetaState.Undefined,
    Map.empty,
    Repo.empty,
    Map.empty,
    ClientAttachments.empty,
    Set.empty,
    Map.empty)

  val empty = Undefined

  def newBuilder() = new ControllerStateBuilder

  protected val inventoryItems = Vector[InventoryItem.Companion_](
    AgentRef, SubagentItem, SubagentSelection, Lock, Board, Calendar, FileWatch, JobResource,
    Workflow, WorkflowPathControl, WorkflowControl)

  lazy val snapshotObjectJsonCodec = TypedJsonCodec[Any](
    Subtype[JournalHeader],
    Subtype[SnapshotMeta],
    Subtype[JournalState],
    Subtype[ClusterStateSnapshot],
    Subtype[ControllerMetaState],
    Subtype[AgentRefState],
    Subtype[SubagentItemState](Nil, aliases = Seq("SubagentRefState")),
    Subtype[SubagentSelection],
    Subtype[LockState],
    Subtype[Board],
    Subtype[Calendar],
    Subtype[Notice],
    NoticePlace.Snapshot.subtype,
    BoardState.NoticeConsumptionSnapshot.subtype,
    Subtype[VersionedEvent],  // These events describe complete objects
    Subtype[InventoryItemEvent],  // For Repo and SignedItemAdded
    Subtype[OrderWatchState.Snapshot],
    Subtype[Order[Order.State]],
    WorkflowPathControl.subtype,
    WorkflowControl.subtype)

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
      KeyedSubtype[BoardEvent])

  private val DummyClusterNodeName = NodeName("DummyControllerNodeName")

  object implicits {
    implicit val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
      ControllerState.snapshotObjectJsonCodec
  }

  final case class WorkflowToOrders(workflowIdToOrders: Map[WorkflowId, Set[OrderId]])
  {
    def moveOrder(order: Order[Order.State], to: WorkflowId): WorkflowToOrders =
      removeOrder(order).addOrder(order.id, to)

    def addOrder(order: Order[Order.State]): WorkflowToOrders =
      addOrder(order.id, order.workflowId)

    private def addOrder(orderId: OrderId, workflowId: WorkflowId): WorkflowToOrders =
      copy(workflowIdToOrders.updated(
        workflowId,
        workflowIdToOrders.getOrElse(workflowId, Set.empty) + orderId))

    def removeOrder(order: Order[Order.State]): WorkflowToOrders = {
      val orderIds = workflowIdToOrders(order.workflowId) - order.id
      if (orderIds.isEmpty)
        copy(workflowIdToOrders - order.workflowId)
      else
        copy(workflowIdToOrders.updated(order.workflowId, orderIds))
    }
  }
}

package js7.data.controller

import cats.syntax.traverse._
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.problem.Checked.{CheckedOption, RichCheckedIterable}
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.data.Problems.{ItemIsStillReferencedProblem, MissingReferencedItemProblem}
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRefStateEvent}
import js7.data.board.BoardEvent.{NoticeDeleted, NoticePosted}
import js7.data.board.{Board, BoardEvent, BoardPath, BoardState, Notice}
import js7.data.calendar.{Calendar, CalendarPath, CalendarState}
import js7.data.cluster.{ClusterEvent, ClusterStateSnapshot}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.controller.ControllerState.logger
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{Event, EventId, ItemContainer, JournalEvent, JournalHeader, JournalState, KeyedEvent, KeyedEventTypedJsonCodec, SignedItemContainer, SnapshotMeta, SnapshotableState}
import js7.data.item.BasicItemEvent.{ItemAttachedStateEvent, ItemDeleted, ItemDeletionMarked, ItemDetachable}
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable, Detached, NotDetached}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, ClientAttachments, InventoryItem, InventoryItemEvent, InventoryItemKey, InventoryItemPath, InventoryItemState, ItemAttachedState, ItemRevision, Repo, SignableItem, SignableItemKey, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, SimpleItem, SimpleItemPath, UnsignedSimpleItem, UnsignedSimpleItemEvent, UnsignedSimpleItemPath, UnsignedSimpleItemState, VersionedEvent, VersionedItemId_, VersionedItemPath}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.OrderNoticesExpected
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatch, OrderWatchEvent, OrderWatchPath, OrderWatchState, OrderWatchStateHandler}
import js7.data.state.EventDrivenStateView
import js7.data.subagent.SubagentItemStateEvent.SubagentShutdown
import js7.data.subagent.{SubagentId, SubagentItem, SubagentItemState, SubagentItemStateEvent, SubagentSelection, SubagentSelectionId, SubagentSelectionState}
import js7.data.value.Value
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath, WorkflowPathControl, WorkflowPathControlPath}
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
  pathToItemState_ : Map[InventoryItemKey, InventoryItemState],
  repo: Repo,
  pathToSignedSimpleItem: Map[SignableSimpleItemPath, Signed[SignableSimpleItem]],
  agentAttachments: ClientAttachments[AgentPath],
  /** Used for OrderWatch to allow to attach it from Agent. */
  deletionMarkedItems: Set[InventoryItemKey],
  idToOrder: Map[OrderId, Order[Order.State]],
  workflowIdToOrders: Map[WorkflowId, Set[OrderId]])
extends SignedItemContainer
with EventDrivenStateView[ControllerState, Event]
with OrderWatchStateHandler[ControllerState]
with SnapshotableState[ControllerState]
{
  def isAgent = false

  def controllerId = controllerMetaState.controllerId

  def companion = ControllerState

  def estimatedSnapshotSize: Int =
    1 +
    standards.snapshotSize +
    controllerMetaState.isDefined.toInt +
    repo.estimatedEventCount +
    pathToItemState_.size +
    pathTo(OrderWatchState).values.view.map(_.estimatedSnapshotSize - 1).sum +
    pathTo(BoardState).values.view.map(_.noticeCount).sum +
    pathToSignedSimpleItem.size +
    agentAttachments.estimatedSnapshotSize +
    deletionMarkedItems.size +
    idToOrder.size

  def toSnapshotObservable: Observable[Any] =
    Observable(
      Observable.pure(SnapshotEventId(eventId)),
      standards.toSnapshotObservable,
      Observable.fromIterable(controllerMetaState.isDefined ? controllerMetaState),
      Observable.fromIterable(pathTo(WorkflowPathControl).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(pathTo(AgentRefState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(pathTo(SubagentItemState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(pathTo(SubagentSelectionState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(pathTo(LockState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(pathTo(BoardState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(pathTo(CalendarState).values).flatMap(_.toSnapshotObservable),
      Observable.fromIterable(pathTo(OrderWatchState).values).flatMap(_.toSnapshotObservable),
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
                          pathToItemState_.insert(agentRef.path, AgentRefState(agentRef))
                        pathToItemState <-
                          maybeSubagentItem match {
                            case None => Right(pathToItemState)
                            case Some(subagentItem) => pathToItemState
                              .insert(subagentItem.id, SubagentItemState.initial(subagentItem))
                          }
                      } yield copy(
                        pathToItemState_ = pathToItemState)
                    }

                case orderWatch: OrderWatch =>
                  ow.addOrderWatch(orderWatch.toInitialItemState)

                case item: UnsignedSimpleItem =>
                  for (o <- pathToItemState_.insert(item.path, item.toInitialItemState)) yield
                    copy(pathToItemState_ = o)
              }

            case UnsignedSimpleItemChanged(item) =>
              item match {
                case changedAgentRef: AgentRef =>
                  changedAgentRef
                    .convertFromV2_1
                    .flatMap { case (agentRef, maybeSubagentItem) =>
                      for {
                        agentRefState <- pathTo(AgentRefState).checked(agentRef.path)
                        _ <- (agentRef.directors == agentRefState.agentRef.directors) !!
                          Problem.pure("Agent Director cannot not be changed")
                        updatedAgentRef <- agentRefState.updateItem(agentRef)
                      } yield
                        copy(
                          pathToItemState_ = pathToItemState_
                            .updated(agentRef.path, updatedAgentRef)
                            .pipeMaybe(maybeSubagentItem)((pathToItemState, changedSubagentItem) =>
                              // COMPATIBLE with v2.2.2
                              pathTo(SubagentItemState)
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
                    itemState <- pathToItemState_.checked(item.path)
                    updated <- itemState.updateItem(item.asInstanceOf[itemState.companion.Item])
                  } yield
                    copy(
                      pathToItemState_ =
                        pathToItemState_.updated(item.path, updated))
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

                case path: UnsignedSimpleItemPath =>
                  path match {
                    case _: AgentPath | _: SubagentId | _: SubagentSelectionId |
                         _: LockPath | _: BoardPath | _: CalendarPath |
                         _: WorkflowPathControlPath =>
                      Right(updated.copy(
                        pathToItemState_ = pathToItemState_ - path))
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
        agentRefState <- pathTo(AgentRefState).checked(agentPath)
        agentRefState <- agentRefState.applyEvent(event)
      } yield copy(
        pathToItemState_ = pathToItemState_ + (agentPath -> agentRefState))

    case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
      applyOrderEvent(orderId, event)

    case KeyedEvent(boardPath: BoardPath, NoticePosted(notice)) =>
      for {
        boardState <- pathTo(BoardState).checked(boardPath)
        o <- boardState.addNotice(notice.toNotice(boardPath))
      } yield copy(
        pathToItemState_ = pathToItemState_.updated(o.path, o))

    case KeyedEvent(boardPath: BoardPath, NoticeDeleted(noticeId)) =>
      for {
        boardState <- pathTo(BoardState).checked(boardPath)
        o <- boardState.removeNotice(noticeId)
      } yield copy(
        pathToItemState_ = pathToItemState_.updated(o.path, o))

    case KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent) =>
      ow.onOrderWatchEvent(orderWatchPath <-: event)

    case KeyedEvent(subagentId: SubagentId, event: SubagentItemStateEvent) =>
      event match {
        case SubagentShutdown if !pathToItemState_.contains(subagentId) =>
          // May arrive when SubagentItem has been deleted
          Right(this)

        case _ =>
          for {
            o <- pathTo(SubagentItemState).checked(subagentId)
            o <- o.applyEvent(event)
          } yield copy(
            pathToItemState_ = pathToItemState_.updated(subagentId, o))
      }

    case KeyedEvent(_, _: ControllerShutDown) =>
      Right(this)

    case KeyedEvent(_, ControllerTestEvent) =>
      Right(this)

    case _ => applyStandardEvent(keyedEvent)
  }

  /** The Agents for each WorkflowPathControl which have not attached the current itemRevision. */
  def workflowPathControlToIgnorantAgents: View[(WorkflowPathControlPath, Set[AgentPath])] =
    pathTo(WorkflowPathControl).values
      .view
      .flatMap { workflowPathControl =>
        val agentPaths = agentAttachments.itemToDelegateToAttachedState
          .get(workflowPathControl.path)
          .view
          .flatMap(_.collect {
            case (agentPath, Attachable) => agentPath
          })
          .toSet
        agentPaths.nonEmpty ? (workflowPathControl.path -> agentPaths)
      }

  def singleWorkflowPathControlToIgnorantAgents(path: WorkflowPathControlPath): Set[AgentPath] =
    agentAttachments.itemToDelegateToAttachedState
      .get(path)
      .view
      .flatMap(_.collect {
        case (agentPath, Attachable) => agentPath
      })
      .toSet

  def orderToAvailableNotices(orderId: OrderId): Seq[Notice] = {
    val pathToBoardState = pathTo(BoardState)
    orderToExpectedNotices(orderId)
      .flatMap(expected =>
        pathToBoardState
          .get(expected.boardPath)
          .flatMap(_
            .idToNotice.get(expected.noticeId)
            .flatMap(_.notice)))
  }

  def orderToStillExpectedNotices(orderId: OrderId): Seq[OrderNoticesExpected.Expected] = {
    val pathToBoardState = pathTo(BoardState)
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

  protected def pathToOrderWatchState = pathTo(OrderWatchState)

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
        pathToItemState_ = pathToItemState_
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
        workflowIdToOrders = updated.addOrderToWorkflowIdToOrders(order))

  private def addOrderToWorkflowIdToOrders(order: Order[Order.State])
  : Map[WorkflowId, Set[OrderId]] =
    workflowIdToOrders.updated(
      order.workflowId,
      workflowIdToOrders.getOrElse(order.workflowId, Set.empty) + order.id)

  override protected def deleteOrder(order: Order[Order.State]): Checked[ControllerState] = {
    val idToOrder_ = idToOrder - order.id
    val workflowToOrders_ = {
      val orderIds = workflowIdToOrders(order.workflowId) - order.id
      if (orderIds.isEmpty)
        workflowIdToOrders - order.workflowId
      else
        workflowIdToOrders.updated(order.workflowId, orderIds)
    }

    order.externalOrderKey match {
      case None =>
        Right(copy(
          idToOrder = idToOrder_,
          workflowIdToOrders = workflowToOrders_))
      case Some(externalOrderKey) =>
        ow
          .onOrderDeleted(externalOrderKey, order.id)
          .map(_.copy(
            idToOrder = idToOrder_,
            workflowIdToOrders = workflowToOrders_))
    }
  }

  /** The named values as seen at the current workflow position. */
  def orderNamedValues(orderId: OrderId): Checked[MapView[String, Value]] =
    for {
      order <- idToOrder.checked(orderId)
      workflow <- repo.idTo[Workflow](order.workflowId)
    } yield order.namedValues(workflow)

  private[controller] def checkAddedOrChangedItems(itemKeys: Iterable[InventoryItemKey]): Checked[Unit] =
    itemKeys
      .flatMap(itemKey =>
        keyToItem.checked(itemKey)
          .flatTraverse(_
            .referencedItemPaths
            .map(path => pathToItem
              .get(path)
              .toChecked(MissingReferencedItemProblem(itemKey, referencedItemKey = path)))
            .toVector))
      .combineProblems
      .rightAs(())

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

  def agentToUri(agentPath: AgentPath): Option[Uri] =
    keyTo(AgentRef)
      .get(agentPath)
      .flatMap(agentRef =>
        agentRef.director match {
          case Some(director) => keyTo(SubagentItem).get(director).map(_.uri)
          case None => agentRef.uri
        })

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
          case path: SimpleItemPath => pathToItem.get(path)
        }

      def iterator = items.map(o => o.key -> o).iterator

      override def values = items
    }

  def pathToItemState: MapView[InventoryItemKey, InventoryItemState] =
    pathToItemState_.view

  private lazy val pathToItem: MapView[InventoryItemPath, InventoryItem] =
    new MapView[InventoryItemPath, InventoryItem] {
      def get(path: InventoryItemPath): Option[InventoryItem] = {
        path match {
          case path: UnsignedSimpleItemPath => pathToItemState_.get(path).map(_.item)
          case path: SignableSimpleItemPath => pathToSignedSimpleItem.get(path).map(_.value)
          case path: VersionedItemPath => repo.pathToVersionedItem(path).toOption
        }
      }

      def iterator: Iterator[(InventoryItemPath, InventoryItem)] =
        simpleItems.view.map(item => item.key -> item).iterator ++
          repo.currentItems.iterator.map(o => o.path -> o)
    }

  def items: View[InventoryItem] =
    simpleItems ++ repo.items

  def simpleItems: View[SimpleItem] =
    unsignedSimpleItems ++ pathToSignedSimpleItem.values.view.map(_.value)

  private def unsignedSimpleItems: View[UnsignedSimpleItem] =
    unsignedSimpleItemStates.map(_.item)

  private def unsignedSimpleItemStates: View[UnsignedSimpleItemState] =
    pathToItemState_.values.view.collect { case o: UnsignedSimpleItemState => o }

  lazy val idToWorkflow: PartialFunction[WorkflowId, Workflow] =
    new PartialFunction[WorkflowId, Workflow] {
      def isDefinedAt(workflowId: WorkflowId) =
        repo.idToSigned[Workflow](workflowId).isRight

      def apply(workflowId: WorkflowId): Workflow =
        repo.idToSigned[Workflow](workflowId).orThrow.value

      override def applyOrElse[K <: WorkflowId, V >: Workflow](workflowId: K, default: K => V): V =
        repo.idToSigned[Workflow](workflowId)
          .fold(_ => default(workflowId), _.value)
    }

  def workflowPathToId(workflowPath: WorkflowPath) =
    repo.pathToId(workflowPath)
      .toRight(UnknownKeyProblem("WorkflowPath", workflowPath.string))

  def pathToJobResource = pathTo(JobResource)

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

  private def signedItems: View[Signed[SignableItem]] =
    pathToSignedSimpleItem.values.view ++
      repo.signedItems

  def orders = idToOrder.values

  def finish: ControllerState =
    copy(
      workflowIdToOrders = calculateWorkflowToOrders)

  private def calculateWorkflowToOrders =
    idToOrder.values.view
      .map(o => o.workflowId -> o.id)
      .toVector
      .groupMap[WorkflowId, OrderId](_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap

  override def toString = s"ControllerState(${EventId.toString(eventId)} ${idToOrder.size} orders, " +
    s"Repo(${repo.currentVersionSize} objects, ...))"
}

object ControllerState
extends SnapshotableState.Companion[ControllerState]
with ItemContainer.Companion[ControllerState]
{
  type StateEvent = Event

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
    Map.empty,
    Map.empty)

  val empty = Undefined

  def newBuilder() = new ControllerStateBuilder

  protected val inventoryItems = Vector(
    AgentRef, SubagentItem, SubagentSelection, Lock, Board, Calendar, FileWatch, JobResource, Workflow, WorkflowPathControl)

  lazy val snapshotObjectJsonCodec = TypedJsonCodec[Any](
    Subtype[JournalHeader],
    Subtype[SnapshotMeta],
    Subtype[JournalState],
    Subtype(deriveCodec[ClusterStateSnapshot]),
    Subtype[ControllerMetaState],
    Subtype[AgentRefState],
    Subtype[SubagentItemState](aliases = Seq("SubagentRefState")),
    Subtype[SubagentSelection],
    Subtype[LockState],
    Subtype[Board],
    Subtype[Calendar],
    Subtype[Notice],
    Subtype[VersionedEvent],  // These events describe complete objects
    Subtype[InventoryItemEvent],  // For Repo and SignedItemAdded
    Subtype[OrderWatchState.Snapshot],
    Subtype[Order[Order.State]],
    WorkflowPathControl.subtype)

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

  object implicits {
    implicit val snapshotObjectJsonCodec = ControllerState.snapshotObjectJsonCodec
  }
}

package js7.data.controller

import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import js7.base.circeutils.CirceUtils.deriveCodec
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
import js7.data.calendar.{Calendar, CalendarPath}
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
import js7.data.item.{BasicItemEvent, ClientAttachments, InventoryItem, InventoryItemEvent, InventoryItemKey, InventoryItemPath, ItemAttachedState, ItemRevision, Repo, SignableItem, SignableItemKey, SignableSimpleItem, SignableSimpleItemPath, SignedItemEvent, SimpleItem, SimpleItemPath, UnsignedSimpleItem, UnsignedSimpleItemEvent, VersionedEvent, VersionedItemId_, VersionedItemPath}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.Order.ExpectingNotice
import js7.data.order.OrderEvent.{OrderAdded, OrderAddedX, OrderCancelled, OrderCoreEvent, OrderDeleted, OrderDeletionMarked, OrderForked, OrderJoined, OrderLockEvent, OrderNoticeEvent, OrderNoticeExpected, OrderNoticePosted, OrderNoticeRead, OrderOrderAdded, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{AllOrderWatchesState, FileWatch, OrderWatch, OrderWatchEvent, OrderWatchPath, OrderWatchState}
import js7.data.state.StateView
import js7.data.subagent.{SubagentId, SubagentRef, SubagentRefState, SubagentRefStateEvent}
import js7.data.value.Value
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
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
  pathToAgentRefState: Map[AgentPath, AgentRefState],
  idToSubagentRefState: Map[SubagentId, SubagentRefState],
  pathToLockState: Map[LockPath, LockState],
  pathToBoardState: Map[BoardPath, BoardState],
  pathToCalendar: Map[CalendarPath, Calendar],
  allOrderWatchesState: AllOrderWatchesState,
  repo: Repo,
  pathToSignedSimpleItem: Map[SignableSimpleItemPath, Signed[SignableSimpleItem]],
  agentAttachments: ClientAttachments[AgentPath],
  /** Used for OrderWatch to allow to attach it from Agent. */
  deletionMarkedItems: Set[InventoryItemKey],
  idToOrder: Map[OrderId, Order[Order.State]])
extends SignedItemContainer
with StateView
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
    pathToAgentRefState.size +
    idToSubagentRefState.size +
    pathToLockState.size +
    pathToBoardState.values.size +
    pathToBoardState.values.view.map(_.notices.size).sum +
    pathToCalendar.values.size +
    allOrderWatchesState.estimatedSnapshotSize +
    pathToSignedSimpleItem.size +
    agentAttachments.estimatedSnapshotSize +
    deletionMarkedItems.size +
    idToOrder.size

  def toSnapshotObservable: Observable[Any] =
    Observable(
      Observable.pure(SnapshotEventId(eventId)),
      standards.toSnapshotObservable,
      Observable.fromIterable(controllerMetaState.isDefined ? controllerMetaState),
      Observable.fromIterable(pathToAgentRefState.values),
      Observable.fromIterable(idToSubagentRefState.values),
      Observable.fromIterable(pathToLockState.values),
      Observable.fromIterable(pathToBoardState.values.view.map(_.toSnapshotObservable))
        .flatten,
      Observable.fromIterable(pathToCalendar.values),
      allOrderWatchesState.toSnapshot,
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
                case lock: Lock =>
                  for (o <- pathToLockState.insert(lock.path -> LockState(lock))) yield
                    copy(pathToLockState = o)

                case agentRef: AgentRef =>
                  for (o <- pathToAgentRefState.insert(agentRef.path -> AgentRefState(agentRef)))
                    yield copy(
                      pathToAgentRefState = o)

                case subagentRef: SubagentRef =>
                  for (o <- idToSubagentRefState.insert(subagentRef.id -> SubagentRefState.initial(subagentRef)))
                    yield copy(
                      idToSubagentRefState = o)

                case orderWatch: OrderWatch =>
                  for (o <- allOrderWatchesState.addOrderWatch(orderWatch)) yield
                    copy(allOrderWatchesState = o)

                case board: Board =>
                  for (o <- pathToBoardState.insert(board.path -> BoardState(board))) yield
                    copy(pathToBoardState = o)

                case calendar: Calendar =>
                  for (o <- pathToCalendar.insert(calendar.path -> calendar)) yield
                    copy(pathToCalendar = o)
              }

            case UnsignedSimpleItemChanged(item) =>
              item match {
                case lock: Lock =>
                  for (lockState <- pathToLockState.checked(lock.path))
                    yield copy(
                      pathToLockState = pathToLockState + (lock.path -> lockState.copy(
                        lock = lock)))

                case agentRef: AgentRef =>
                  for {
                    agentRefState <- pathToAgentRefState.checked(agentRef.path)
                    _ <-
                      if (agentRef.directors != agentRefState.agentRef.directors)
                        Left(Problem.pure("Agent Director cannot not be changed"))
                      else
                        Checked.unit
                  } yield copy(
                    pathToAgentRefState = pathToAgentRefState + (agentRef.path -> agentRefState.copy(
                      agentRef = agentRef)))

                case subagentRef: SubagentRef =>
                  for {
                    subagentRefState <- idToSubagentRefState.checked(subagentRef.id)
                    _ <- subagentRefState.subagentRef.agentPath == subagentRef.agentPath !!
                      Problem.pure("A Subagent's AgentPath cannot be changed")
                  } yield copy(
                    idToSubagentRefState = idToSubagentRefState + (subagentRef.id ->
                      subagentRefState.copy(subagentRef = subagentRef)))

                case orderWatch: OrderWatch =>
                  allOrderWatchesState.changeOrderWatch(orderWatch)
                    .map(o => copy(
                      allOrderWatchesState = o))

                case board: Board =>
                  for (boardState <- pathToBoardState.checked(board.path))
                    yield copy(
                      pathToBoardState = pathToBoardState + (board.path -> boardState.copy(
                        board = board)))

                case calendar: Calendar =>
                  for (_ <- pathToCalendar.checked(calendar.path))
                    yield copy(
                      pathToCalendar = pathToCalendar + (calendar.path -> calendar))
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
                case id: VersionedItemId_ =>
                  for (repo <- repo.deleteItem(id)) yield
                    updated.copy(
                      repo = repo)

                case lockPath: LockPath =>
                  Right(updated.copy(
                    pathToLockState = pathToLockState - lockPath))

                case agentPath: AgentPath =>
                  Right(updated.copy(
                    pathToAgentRefState = pathToAgentRefState - agentPath))

                case subagentId: SubagentId =>
                  Right(updated.copy(
                    idToSubagentRefState = idToSubagentRefState - subagentId))

                case path: OrderWatchPath =>
                  Right(updated.copy(
                    allOrderWatchesState = allOrderWatchesState.removeOrderWatch(path)))

                case boardPath: BoardPath =>
                  Right(updated.copy(
                    pathToBoardState = pathToBoardState - boardPath))

                case calendarPath: CalendarPath =>
                  Right(updated.copy(
                    pathToCalendar = pathToCalendar - calendarPath))

                case jobResourcePath: JobResourcePath =>
                  Right(updated.copy(
                    pathToSignedSimpleItem = pathToSignedSimpleItem - jobResourcePath))

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
        agentRefState <- pathToAgentRefState.checked(agentPath)
        agentRefState <- agentRefState.applyEvent(event)
      } yield copy(
        pathToAgentRefState = pathToAgentRefState + (agentPath -> agentRefState))

    case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
      event match {
        case orderAdded: OrderAdded =>
          addOrder(orderId, orderAdded)

        case event: OrderCoreEvent =>
          for {
            previousOrder <- idToOrder.checked(orderId)
            updatedOrder <- previousOrder.applyEvent(event)
            updatedIdToOrder = idToOrder + (updatedOrder.id -> updatedOrder)
            updatedControllerState <- event match {
              case event: OrderForked =>
                Right(copy(
                  idToOrder = updatedIdToOrder ++
                    previousOrder
                      .newForkedOrders(event)
                      .map(childOrder => childOrder.id -> childOrder)))

              case event: OrderJoined =>
                previousOrder.state match {
                  case forked: Order.Forked =>
                    Right(copy(
                      idToOrder = updatedIdToOrder -- forked.childOrderIds))

                  case state =>
                    Left(Problem(
                      s"For event $event, $orderId must be in state Forked or Forked, not: $state"))
                }

              case event: OrderLockEvent =>
                event.lockPaths
                  .toList
                  .traverse(lockPath => pathToLockState(lockPath).applyEvent(orderId <-: event))
                  .map(lockStates =>
                    copy(
                      idToOrder = updatedIdToOrder,
                      pathToLockState = pathToLockState ++ lockStates.map(o => o.lock.path -> o)))

              case event: OrderNoticeEvent =>
                orderIdToBoardState(orderId)
                  .flatMap { boardState =>
                    val boardPath = boardState.path
                    event match {
                      case OrderNoticePosted(notice) =>
                        for (updatedBoardState <- boardState.addNotice(notice)) yield
                          copy(
                            idToOrder = updatedIdToOrder,
                            pathToBoardState = pathToBoardState +
                              (boardPath -> updatedBoardState))

                      case OrderNoticeExpected(noticeId) =>
                        boardState
                          .addExpectation(orderId, noticeId)
                          .map(boardState => copy(
                            idToOrder = updatedIdToOrder,
                            pathToBoardState = pathToBoardState + (boardPath -> boardState)))

                      case OrderNoticeRead =>
                        Right(copy(
                          idToOrder = updatedIdToOrder))
                    }
                  }

              case _: OrderCancelled =>
                previousOrder
                  .ifState[ExpectingNotice].map(order =>
                    for {
                      boardState <- orderIdToBoardState(orderId)
                      updatedBoardState <- boardState.removeExpectation(orderId, order.state.noticeId)
                    } yield
                      copy(
                        idToOrder = updatedIdToOrder,
                        pathToBoardState = pathToBoardState + (boardState.path -> updatedBoardState)))
                  .getOrElse(
                    Right(copy(
                      idToOrder = updatedIdToOrder)))

              case orderAdded: OrderOrderAdded =>
                for (updated <- addOrder(orderAdded.orderId, orderAdded)) yield
                  updated.copy(idToOrder = updated.idToOrder ++ updatedIdToOrder)

              case OrderDeletionMarked =>
                Right(copy(idToOrder = updatedIdToOrder))

              case OrderDeleted =>
                previousOrder.externalOrderKey match {
                  case None =>
                    Right(copy(idToOrder = idToOrder - orderId))
                  case Some(externalOrderKey) =>
                    allOrderWatchesState
                      .onOrderDeleted(externalOrderKey, orderId)
                      .map(o => copy(
                        idToOrder = idToOrder - orderId,
                        allOrderWatchesState = o))
                }

              case _ => Right(copy(idToOrder = updatedIdToOrder))
            }
          } yield updatedControllerState

        case _: OrderStdWritten =>
          Right(this)
      }

    case KeyedEvent(boardPath: BoardPath, NoticePosted(noticeId)) =>
      for {
        boardState <- pathToBoardState.checked(boardPath)
        o <- boardState.addNotice(noticeId)
      } yield copy(
        pathToBoardState = pathToBoardState + (o.path -> o))

    case KeyedEvent(boardPath: BoardPath, NoticeDeleted(noticeId)) =>
      for {
        boardState <- pathToBoardState.checked(boardPath)
        o <- boardState.deleteNotice(noticeId)
      } yield copy(
        pathToBoardState = pathToBoardState + (o.path -> o))

    case KeyedEvent(orderWatchPath: OrderWatchPath, event: OrderWatchEvent) =>
      allOrderWatchesState
        .onOrderWatchEvent(orderWatchPath <-: event)
        .map(o => copy(allOrderWatchesState = o))

    case KeyedEvent(subagentId: SubagentId, event: SubagentRefStateEvent) =>
      for {
        o <- idToSubagentRefState.checked(subagentId)
        o <- o.applyEvent(event)
      } yield copy(
        idToSubagentRefState = idToSubagentRefState + (subagentId -> o))

    case KeyedEvent(_, _: ControllerShutDown) =>
      Right(this)

    case KeyedEvent(_, ControllerTestEvent) =>
      Right(this)

    case _ => applyStandardEvent(keyedEvent)
  }

  private def addOrder(addedOrderId: OrderId, orderAdded: OrderAddedX): Checked[ControllerState] =
    idToOrder.checkNoDuplicate(addedOrderId) >>
      allOrderWatchesState.onOrderAdded(addedOrderId <-: orderAdded)
        .map(updated => copy(
          idToOrder = idToOrder +
            (addedOrderId -> Order.fromOrderAdded(addedOrderId, orderAdded)),
          allOrderWatchesState = updated))

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
          case Some(director) => keyTo(SubagentRef).get(director).map(_.uri)
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
          case path: SimpleItemPath => keyToItemFunc(path).collect { case o: SimpleItem => o }
        }

      def iterator = items.map(o => o.key -> o).iterator

      override def values = items
    }

  private lazy val pathToItem: MapView[InventoryItemPath, InventoryItem] =
    new MapView[InventoryItemPath, InventoryItem] {
      def get(path: InventoryItemPath): Option[InventoryItem] = {
        path match {
          case path: SimpleItemPath =>
            keyToItemFunc(path) collect { case o: SimpleItem => o }

          case path: VersionedItemPath =>
            repo.pathToItem(path).toOption
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
    pathToCalendar.values.view ++
      (idToSubagentRefState.view ++
        pathToAgentRefState.view ++
        pathToLockState ++
        allOrderWatchesState.pathToOrderWatchState
      ).map(_._2.item)

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

  private def signedItems: View[Signed[SignableItem]] =
    pathToSignedSimpleItem.values.view ++
      repo.signedItems

  private def keyToItemFunc(itemKey: InventoryItemKey): Option[InventoryItem] =
    itemKey match {
      case id: VersionedItemId_ => repo.anyIdToSigned(id).toOption.map(_.value)
      case path: AgentPath => pathToAgentRefState.get(path).map(_.item)
      case id: SubagentId => idToSubagentRefState.get(id).map(_.item)
      case path: LockPath => pathToLockState.get(path).map(_.item)
      case path: OrderWatchPath => allOrderWatchesState.pathToOrderWatchState.get(path).map(_.item)
      case path: BoardPath => pathToBoardState.get(path).map(_.item)
      case path: CalendarPath => pathToCalendar.get(path)
      case path: SignableSimpleItemPath => pathToSignedSimpleItem.get(path).map(_.value)
    }

  def orders = idToOrder.values

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
    Map.empty,
    Map.empty,
    Map.empty,
    Map.empty,
    AllOrderWatchesState.empty,
    Repo.empty,
    Map.empty,
    ClientAttachments.empty,
    Set.empty,
    Map.empty)

  val empty = Undefined

  def newBuilder() = new ControllerStateBuilder

  protected val inventoryItems = Vector(
    AgentRef, SubagentRef, Lock, Board, Calendar, FileWatch, JobResource, Workflow)

  lazy val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec.named("ControllerState.snapshotObjectJsonCodec",
      Subtype[JournalHeader],
      Subtype[SnapshotMeta],
      Subtype[JournalState],
      Subtype(deriveCodec[ClusterStateSnapshot]),
      Subtype[ControllerMetaState],
      Subtype[AgentRefState],
      Subtype[SubagentRefState],
      Subtype[LockState],
      Subtype[Board],
      Subtype[Calendar],
      Subtype.named[Notice.Snapshot]("Notice"),
      Subtype[VersionedEvent],  // These events describe complete objects
      Subtype[InventoryItemEvent],  // For Repo and SignedItemAdded
      Subtype[OrderWatchState.Snapshot],
      Subtype[Order[Order.State]])

  implicit lazy val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec.named("ControllerState.keyedEventJsonCodec",
      KeyedSubtype[JournalEvent],
      KeyedSubtype[InventoryItemEvent],
      KeyedSubtype[VersionedEvent],
      KeyedSubtype[ControllerEvent],
      KeyedSubtype[ClusterEvent],
      KeyedSubtype[AgentRefStateEvent],
      KeyedSubtype[SubagentRefStateEvent],
      KeyedSubtype[OrderWatchEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype[BoardEvent])

  object implicits {
    implicit val snapshotObjectJsonCodec = ControllerState.snapshotObjectJsonCodec
  }
}

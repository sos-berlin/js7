package js7.data.controller

import cats.syntax.apply.*
import cats.syntax.traverse.*
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.AgentResetProblem
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.AgentResetStarted
import js7.data.controller.ControllerStateExecutor.*
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttachedStateEvent, ItemDeleted, ItemDetachable, ItemDetached}
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable}
import js7.data.item.VersionedEvent.VersionedItemEvent
import js7.data.item.{InventoryItem, InventoryItemEvent, InventoryItemKey, SimpleItemPath}
import js7.data.job.JobResource
import js7.data.lock.LockState
import js7.data.order.Order.State
import js7.data.order.OrderEvent.{OrderAdded, OrderAwoke, OrderBroken, OrderCoreEvent, OrderDeleted, OrderDetached, OrderForked, OrderLockEvent, OrderMoved, OrderOrderAdded, OrderProcessed}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, Outcome}
import js7.data.orderwatch.ExternalOrderKey
import js7.data.subagent.SubagentItemState
import js7.data.subagent.SubagentItemStateEvent.SubagentReset
import js7.data.value.expression.scopes.NowScope
import js7.data.workflow.WorkflowControlId.syntax.*
import js7.data.workflow.position.{Position, PositionOrLabel}
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowControlId, WorkflowId, WorkflowPathControl, WorkflowPathControlPath}
import scala.annotation.tailrec
import scala.collection.compat.immutable.ArraySeq
import scala.collection.{View, mutable}
import scala.language.implicitConversions

final case class ControllerStateExecutor private(
  keyedEvents: Seq[AnyKeyedEvent],
  controllerState: ControllerState)
  (implicit instructionExecutorService: InstructionExecutorService)
{
  import ControllerStateExecutor.convertImplicitly
  import controllerState.controllerId

  // Same clock time for a chunk of operations
  private lazy val nowScope = NowScope()

  def addOrders(freshOrders: Seq[FreshOrder], suppressOrderIdCheckFor: Option[String] = None)
  : Checked[Seq[KeyedEvent[OrderAdded]]] =
    freshOrders.checkUniqueness(_.id) *>
      freshOrders
        .traverse(addOrder(_, suppressOrderIdCheckFor = suppressOrderIdCheckFor))
        .map(_.flatten)

  def addOrder(
    order: FreshOrder,
    externalOrderKey: Option[ExternalOrderKey] = None,
    suppressOrderIdCheckFor: Option[String] = None)
  : Checked[Option[KeyedEvent[OrderAdded]]] =
    ( if (suppressOrderIdCheckFor.contains(order.id.string)) Checked.unit
      else order.id.checkedNameSyntax
    ) *>
      addOrderWithPrecheckedId(order, externalOrderKey)

  private def addOrderWithPrecheckedId(
    freshOrder: FreshOrder,
    externalOrderKey: Option[ExternalOrderKey])
  : Checked[Option[KeyedEvent[OrderAdded]]] =
    if (controllerState.idToOrder.contains(freshOrder.id))
      Right(None) // Ignore known orders  — TODO Fail as duplicate if deleteWhenTerminated ?
    else
      for {
        workflow <- controllerState.repo.pathTo(Workflow)(freshOrder.workflowPath)
        preparedArguments <- workflow.orderParameterList.prepareOrderArguments(
          freshOrder, controllerId, controllerState.keyToItem(JobResource), nowScope)
        startPosition <- freshOrder.startPosition.traverse(checkStartOrStopPosition(_, workflow))
        _ <- freshOrder.stopPositions.toSeq.traverse(checkStartOrStopPosition(_, workflow))
      } yield Some(
        freshOrder.toOrderAdded(workflow.id.versionId, preparedArguments, externalOrderKey,
          startPosition))

  private def checkStartOrStopPosition(positionOrLabel: PositionOrLabel, workflow: Workflow)
  : Checked[Position] =
    for {
      position <- workflow.positionOrLabelToPosition(positionOrLabel)
      pos <- workflow.checkedPosition(position)
      _ <- workflow.isMoveable(Position.First, position) !! Problem(
        s"Order's startPosition or one of its stopPositions is not reachable: $positionOrLabel")
    } yield pos

  def resetAgent(agentPath: AgentPath, force: Boolean): Checked[Seq[AnyKeyedEvent]] = {
    val agentResetStarted = View(agentPath <-: AgentResetStarted(force = force))

    val subagentReset = controllerState
      .keyTo(SubagentItemState).values.view
      .filter(_.item.agentPath == agentPath)
      .map(_.path <-: SubagentReset)
      .toVector

    val itemsDetached = controllerState.itemToAgentToAttachedState.to(View)
      .filter(_._2.contains(agentPath))
      .map(_._1)
      .map(itemKey => NoKey <-: ItemDetached(itemKey, agentPath))

    controllerState
      .idToOrder.values.view
      .filter(_.isAtAgent(agentPath))
      .map(forciblyDetachOrder(_, agentPath))
      .toVector.sequence
      .map(_.view.flatten)
      .map(ordersDetached =>
        agentResetStarted ++ subagentReset ++ ordersDetached ++ itemsDetached)
      .map(_.toVector)
  }

  private def forciblyDetachOrder(order: Order[Order.State], agentPath: AgentPath)
  : Checked[Seq[KeyedEvent[OrderCoreEvent]]] = {
    val outcome = Outcome.Disrupted(AgentResetProblem(agentPath))
    val stateEvents = (order.state: State) match {
      case _: Order.Processing =>
        Vector(order.id <-: OrderProcessed(outcome))

      case _: Order.DelayedAfterError =>
        Vector(order.id <-: OrderAwoke)

      case _ => Vector.empty
    }
    val detached = stateEvents :+ (order.id <-: OrderDetached)
    for {
      detachedState <- controllerState.applyEvents(detached)
      fail <- new OrderEventSource(detachedState)
        .fail(detachedState.idToOrder(order.id), Some(outcome), uncatchable = true)
    } yield detached ++ fail.view.map(order.id <-: _)
  }

  def applyEventsAndReturnSubsequentEvents(keyedEvents: Iterable[AnyKeyedEvent])
  : Checked[ControllerStateExecutor] =
    for {
      tuple <- applyEvents(keyedEvents)
      result <- (subsequentEvents _).tupled(tuple)
    } yield result

  private def applyEvents(keyedEvents: Iterable[AnyKeyedEvent])
  : Checked[(
    collection.Set[InventoryItemKey],
    collection.Set[OrderId],
    collection.Set[WorkflowId],
    collection.Set[InventoryItemKey],
    collection.Seq[(WorkflowId, AgentPath)],
    collection.Seq[WorkflowId],
    ControllerState)] =
  {
    val touchedItemKeys = mutable.Set.empty[InventoryItemKey]
    val touchedOrderIds = mutable.Set.empty[OrderId]
    val detachWorkflowCandidates = mutable.Set.empty[WorkflowId]
    val detachedItems = mutable.Set.empty[InventoryItemKey]
    val detachedWorkflows = mutable.Buffer.empty[(WorkflowId, AgentPath)]
    val deletedWorkflows = mutable.Buffer.empty[WorkflowId]

    var controllerState = this.controllerState
    var checked: Checked[Unit] = Checked.unit
    val iterator = keyedEvents.iterator

    while (iterator.hasNext && checked.isRight) {
      val keyedEvent = iterator.next()
      val previous = controllerState

      controllerState.applyEvent(keyedEvent) match {
        case Left(problem) =>
          checked = Left(problem)

        case Right(updated) =>
          controllerState = updated
          keyedEvent match {
            case KeyedEvent(_, event: VersionedItemEvent) =>
              for (previousItem <- previous.repo.pathToVersionedItem(event.path)) {
                touchedItemKeys += previousItem.id
                previousItem match {
                  case previousItem: Workflow =>
                    detachWorkflowCandidates += previousItem.id
                  case _ =>
                }
              }

            case KeyedEvent(_: NoKey, event: InventoryItemEvent) =>
              touchedItemKeys += event.key

              event match {
                case ItemDetached(itemKey, agentPath: AgentPath) =>
                  detachedItems += itemKey
                  itemKey match {
                    case WorkflowId.as(workflowId: WorkflowId) =>
                      detachedWorkflows += workflowId -> agentPath
                    case _ =>
                  }

                case ItemDeleted(WorkflowId.as(workflowId)) =>
                  deletedWorkflows += workflowId

                case _ =>
              }

            case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
              touchedOrderIds += orderId
              event match {
                case OrderDeleted =>
                  detachWorkflowCandidates += previous.idToOrder(orderId).workflowId
                case _ =>
              }

            case _ =>
          }
      }
    }

    for (_ <- checked) yield
      (touchedItemKeys, touchedOrderIds, detachWorkflowCandidates, detachedItems, detachedWorkflows,
        deletedWorkflows, controllerState)
  }

  private def subsequentEvents(
    touchedItemKeys: Iterable[InventoryItemKey],
    touchedOrderIds: Iterable[OrderId],
    detachWorkflowCandidates: Iterable[WorkflowId],
    detachedItems1: Iterable[InventoryItemKey],
    detachedWorkflows: Iterable[(WorkflowId, AgentPath)],
    deletedWorkflows: Iterable[WorkflowId],
    controllerState: ControllerState)
  : Checked[ControllerStateExecutor] = {
    // Slow ???
    val detachedItems = mutable.Set.empty[InventoryItemKey] ++ detachedItems1
    val controllerStateBeforeSubsequentEvents = controllerState
    val itemEvents = ControllerStateExecutor(controllerState)
      .nextItemAttachedStateEvents(touchedItemKeys)
      .toVector

    controllerState
      .applyEvents(itemEvents)
      .flatMap { controllerState_ =>
        var controllerState = controllerState_

        val detachWorkflows: Seq[KeyedEvent[ItemDetachable]] =
          detachWorkflowCandidates
            .view
            .filter(controllerState.keyToItem.keySet.contains)
            .filter(controllerState.isObsoleteItem)
            .flatMap(workflowId =>
              controllerState.itemToAgentToAttachedState
                .get(workflowId)
                .view
                .flatMap(_.collect {
                  // Workflows are never Attachable, only Attached or Detachable
                  case (agentPath, Attached(_)) =>
                    detachedItems += workflowId
                    NoKey <-: ItemDetachable(workflowId, agentPath)
                }))
            .toVector

        val detachWorkflowControls: Seq[KeyedEvent[ItemDetachable]] =
          detachedWorkflows.view
            .flatMap { case (workflowId, agentPath) =>
              val workflowControlId = WorkflowControlId(workflowId)
              controllerState.itemToAgentToAttachedState.get(workflowControlId)
                .flatMap(_.get(agentPath))
                .exists(_.isAttachableOrAttached)
                .thenList(NoKey <-: ItemDetachable(workflowControlId, agentPath))
            }
            .toVector

        val deleteItems0: Set[KeyedEvent[ItemDeleted]] =
          (detachWorkflowCandidates.view ++ detachedItems)
            .filter(controllerState.keyToItem.contains)
            .filterNot(controllerState.itemToAgentToAttachedState.contains)
            .filter {
              case WorkflowId.as(workflowId) =>
                controllerState.isObsoleteItem(workflowId)

              case path: WorkflowPathControlPath =>
                // Delete when no Workflow version exists
                !controllerState.repo.pathToItems(Workflow).contains(path.workflowPath)

              case WorkflowControlId.as(workflowControlId) =>
                // Delete when Workflow no longer exists
                !controllerState.idToWorkflow.isDefinedAt(workflowControlId.workflowPath)

              case path: SimpleItemPath =>
                !controllerState.isReferenced(path)

              case _ =>
                false
            }
            .map(itemKey => NoKey <-: ItemDeleted(itemKey))
            .toSet

        val deleteControls1: View[KeyedEvent[ItemDeleted]] =
          detachedItems
            .view
            .filterNot(controllerState.keyToItem.contains)
            .filter {
              case path: WorkflowPathControlPath =>
                // Delete when no Workflow version exists
                !controllerState.repo.pathToItems(Workflow).contains(path.workflowPath)

              case WorkflowControlId.as(workflowControlId) =>
                // Delete when Workflow no longer exists
                !controllerState.idToWorkflow.isDefinedAt(workflowControlId.workflowPath)

              case _ =>
                false
            }
            .map(itemKey => NoKey <-: ItemDeleted(itemKey))

        val deleteControls2: View[KeyedEvent[ItemDeleted]] =
          deletedWorkflows
            .view
            .filterNot(controllerState.keyToItem.contains)
            .flatMap {
              case WorkflowId.as(workflowId) =>
                // Delete when no Workflow version exists
                val deleteWorkflowPathControl = {
                  val workflowPathControlPath = WorkflowPathControlPath(workflowId.path)
                  (controllerState.keyTo(WorkflowPathControl).contains(workflowPathControlPath) &&
                    !controllerState.itemToAgentToAttachedState.contains(workflowPathControlPath) &&
                    !controllerState.repo.pathToItems(Workflow).contains(workflowId.path)
                  ) ? (NoKey <-: ItemDeleted(workflowPathControlPath))
                }

                // Delete WorkflowControl when Workflow no longer exists
                val deleteWorkflowControl = {
                  val workflowControlId = WorkflowControlId(workflowId)
                  (controllerState.keyTo(WorkflowControl).contains(workflowControlId) &&
                    !controllerState.itemToAgentToAttachedState.contains(workflowControlId) &&
                    !controllerState.idToWorkflow.isDefinedAt(workflowControlId.workflowPath)
                  ) ? (NoKey <-: ItemDeleted(workflowControlId))
                }

                deleteWorkflowPathControl ++ deleteWorkflowControl

              case _ =>
                Nil
            }

        val deleteItems: Set[KeyedEvent[ItemDeleted]] =
          deleteItems0 ++ deleteControls1 ++ deleteControls2

        controllerState = controllerState
          .applyEvents(detachWorkflows.view ++ detachWorkflowControls ++ deleteItems)
          .orThrow

        val eventsAndState = controllerState.nextOrderEvents(touchedOrderIds)
        controllerState = eventsAndState.controllerState
        val orderEvents = eventsAndState.keyedEvents

        val orderWatchEvents = controllerState.nextOrderWatchOrderEvents
        controllerState = controllerState.applyEvents(orderWatchEvents)
          .orThrow

        val subsequentKeyedEvents = Vector.concat(
          itemEvents,
          detachWorkflows,
          detachWorkflowControls,
          deleteItems,
          orderEvents,
          orderWatchEvents)

        // Loop to derive further events from the just derived events
        if (subsequentKeyedEvents.nonEmpty)
          controllerStateBeforeSubsequentEvents
            .applyEventsAndReturnSubsequentEvents(subsequentKeyedEvents)
            .map(o => ControllerStateExecutor(subsequentKeyedEvents ++ o.keyedEvents, o.controllerState))
        else
          Right(new ControllerStateExecutor(subsequentKeyedEvents, controllerState))
    }
  }

  private def nextItemAttachedStateEvents(itemKeys: Iterable[InventoryItemKey])
  : View[KeyedEvent[ItemAttachedStateEvent]] =
    itemKeys
      .view
      .flatMap(controllerState.keyToItem.get)
      .flatMap(nextItemAttachedStateEventsForItem)
      .map(NoKey <-: _)

  private def nextItemAttachedStateEventsForItem(item: InventoryItem)
  : Iterable[ItemAttachedStateEvent] =
    controllerState.itemToAgentToAttachedState.get(item.key) match {
      case Some(agentPathToAttached) =>
        agentPathToAttached.view.flatMap {
          case (agentPath, Attached(revision)) =>
            def detachEvent = detach(item.key, agentPath)

            item.key match {
              case WorkflowId.as(workflowId) =>
                if (controllerState.isObsoleteItem(workflowId))
                  derivedWorkflowPathControlEvent(workflowId, agentPath).toList ++
                    derivedWorkflowControlEvent(workflowId, agentPath) :+
                    detachEvent
                else
                  Nil

              case itemKey =>
                if (controllerState.deletionMarkedItems contains itemKey)
                  detachEvent :: Nil
                else
                  (item.itemRevision != revision).thenList(
                    (item.dedicatedAgentPath match {
                      case Some(`agentPath`) | None =>
                        // Item is dedicated to this Agent or is required by an Order.
                        // Attach again without detaching, and let Agent change the item while in flight
                        ItemAttachable(itemKey, agentPath)
                      case Some(_) =>
                        // Item's Agent dedication has changed, so we detach it
                        detachEvent
                    }))
            }

          case (_, Attachable | Detachable) =>
            Nil
        }

      case None =>
        if (controllerState.deletionMarkedItems.contains(item.key))
          Nil
        else
          item.dedicatedAgentPath.map(ItemAttachable(item.key, _))
    }

  private def derivedWorkflowPathControlEvent(workflowId: WorkflowId, agentPath: AgentPath)
  : Option[ItemAttachedStateEvent] = {
    // Implicitly detach WorkflowPathControl from agentPath
    // when last version of WorkflowPath has been detached
    val otherWorkflowWillStillBeAttached = controllerState.repo
      .pathToItems(Workflow)(workflowId.path)
      .map(_.id)
      .filter(_ != workflowId) // workflowId is going to be detached
      .exists(workflowId => controllerState
        .itemToAgentToAttachedState.get(workflowId)
        .exists(_ contains agentPath))

      if (otherWorkflowWillStillBeAttached)
        None
      else {
        val controlPath = WorkflowPathControlPath(workflowId.path)
        controllerState.itemToAgentToAttachedState
          .get(controlPath)
          .flatMap(_
            .get(agentPath)
            .collect {
              case Attachable | Attached(_) => detach(controlPath, agentPath)
            })
      }
  }

  private def derivedWorkflowControlEvent(workflowId: WorkflowId, agentPath: AgentPath)
  : Option[ItemAttachedStateEvent] =
    // Implicitly detach WorkflowControl from agentPath when Workflow has been detached
    if (controllerState.itemToAgentToAttachedState.contains(workflowId))
      None
    else {
      val controlId = WorkflowControlId(workflowId)
      controllerState.itemToAgentToAttachedState
        .get(controlId)
        .flatMap(_
          .get(agentPath)
          .collect {
            case Attachable | Attached(_) => detach(controlId, agentPath)
          })
    }

  private def detach(itemKey: InventoryItemKey, agentPath: AgentPath): ItemAttachedStateEvent =
    if (controllerState.keyToUnsignedItemState_.contains(agentPath))
      ItemDetachable(itemKey, agentPath)
    else // shortcut in case, the Agent has been deleted (reset)
      ItemDetached(itemKey, agentPath)

  def updatedWorkflowPathControlAttachedEvents(workflowPathControl: WorkflowPathControl)
  : Iterable[ItemAttachable] =
    controllerState
      .repo.pathToItems(Workflow)
      .getOrElse(workflowPathControl.workflowPath, View.empty)
      .view
      .flatMap(workflow =>
        controllerState.itemToAgentToAttachedState
          .getOrElse(workflow.id, Map.empty)
          .collect {
            case (agentPath, Attachable | Attached(_)) => agentPath
          })
      .toSet
      .map(ItemAttachable(workflowPathControl.path, _))

  def updatedWorkflowControlAttachedEvents(workflowControl: WorkflowControl)
  : Iterable[ItemAttachable] =
    controllerState
      .repo.idTo(Workflow)(workflowControl.workflowId)
      .toOption.view
      .flatMap(workflow =>
        controllerState.itemToAgentToAttachedState
          .getOrElse(workflow.id, Map.empty)
          .collect {
            case (agentPath, Attachable | Attached(_)) => agentPath
          })
      .toSet
      .map(ItemAttachable(workflowControl.id, _))

  def nextOrderWatchOrderEvents: View[KeyedEvent[OrderCoreEvent]] =
    controllerState.ow.nextEvents(addOrder(_, _), isDeletionMarkable)

  private def isDeletionMarkable(orderId: OrderId): Boolean =
    controllerState.idToOrder.get(orderId).exists(o => !o.deleteWhenTerminated)

  def nextOrderEvents(orderIds: Iterable[OrderId]): ControllerStateExecutor = {
    var controllerState = this.controllerState
    val queue = mutable.Queue.empty[OrderId] ++= orderIds
    val _keyedEvents = Vector.newBuilder[KeyedEvent[OrderCoreEvent]]

    @tailrec def loop(): Unit =
      queue.removeHeadOption() match {
        case Some(orderId) =>
          if (controllerState.idToOrder contains orderId) {
            val keyedEvents = new OrderEventSource(controllerState).nextEvents(orderId)
            for (case KeyedEvent(orderId, OrderBroken(maybeProblem)) <- keyedEvents) {
              logger.error(s"$orderId is broken: ${maybeProblem getOrElse "None"}") // ???
            }
            controllerState.applyEvents(keyedEvents) match {
              case Left(problem) =>
                logger.error(s"$orderId: $problem")  // Should not happen
              case Right(state) =>
                controllerState = state
                _keyedEvents ++= keyedEvents
                queue ++= keyedEvents.view
                  .flatMap(ControllerStateExecutor(controllerState).keyedEventToOrderIds)
                  .toSeq.distinct
            }
          }
          loop()
        case None =>
      }

    loop()
    new ControllerStateExecutor(
      /*optimizeKeyedEvents*/(_keyedEvents.result()),
      controllerState)
  }

  private def optimizeKeyedEvents[E <: Event](keyedEvents: Seq[KeyedEvent[E]]): Seq[KeyedEvent[E]] = {
    val orderToIndex = mutable.Map[OrderId, Int]()
    val buffer = mutable.ArrayBuffer[KeyedEvent[E]]()
    buffer.sizeHint(keyedEvents.size)
    keyedEvents.foreach {
      case ke @ KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        def add() = {
          orderToIndex(orderId) = buffer.length
          buffer += ke
        }
        event match {
          case OrderMoved(_, None) =>
            orderToIndex.get(orderId) match {
              case None => add()
              case Some(i) =>
                buffer(i) match {
                  case KeyedEvent(_, OrderMoved(_, None)) =>
                    buffer(i) = ke
                  case _ => add()
                }
            }

          case _ => add()
        }

      case ke =>
        buffer += ke
    }
    buffer.to(ArraySeq)
  }

  private def keyedEventToOrderIds(keyedEvent: KeyedEvent[OrderEvent]): View[OrderId] =
    View(keyedEvent.key) ++ (keyedEvent.event match {
      case OrderLockEvent(lockPaths) =>
        lockPaths.view
          .flatMap(controllerState.keyTo(LockState).get)
          .flatMap(_.firstQueuedOrderId)

      case OrderForked(children) =>
        children.view.map(_.orderId)

      case orderOrderAdded: OrderOrderAdded =>
        new View.Single(orderOrderAdded.orderId)

      case _ => View.empty
    })
}

object ControllerStateExecutor
{
  private val logger = Logger[this.type]

  def apply(controllerState: ControllerState)(implicit service: InstructionExecutorService)
  : ControllerStateExecutor =
    new ControllerStateExecutor(Nil, controllerState)(service)

  implicit def convertImplicitly(controllerState: ControllerState)
    (implicit ies: InstructionExecutorService)
  : ControllerStateExecutor =
    apply(controllerState)
}

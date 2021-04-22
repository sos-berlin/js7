package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.item.{ItemAttachedState, SimpleItemState, VersionId}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderRemoveMarked, OrderRemoved}
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState._
import js7.data.value.NamedValues
import js7.data.workflow.WorkflowPath
import monix.reactive.Observable
import scala.collection.View

final case class OrderWatchState(
  orderWatch: OrderWatch,
  agentIdToAttachedState: Map[AgentPath, ItemAttachedState.NotDetached],
  delete: Boolean,
  externalToState: Map[ExternalOrderName, ArisedOrHasOrder],
  private[orderwatch] val arisedQueue: Set[ExternalOrderName],
  private[orderwatch] val vanishedQueue: Set[ExternalOrderName])
extends SimpleItemState
{
  def item = orderWatch

  def id: OrderWatchPath = orderWatch.id

  type Revision = Int

  private def recoverQueues: OrderWatchState =
    copy(
      arisedQueue = externalToState.view
        .collect {
          case (externalOrderName, Arised(_, _)) => externalOrderName
        }
        .toSet,
      vanishedQueue = externalToState.view
        .collect {
          case (externalOrderName, HasOrder(_, Some(Vanished))) => externalOrderName
        }
        .toSet)

  def isDestroyable =
    delete && agentIdToAttachedState.isEmpty

  def applyOrderWatchEvent(event: OrderWatchEvent): Checked[OrderWatchState] =
    event match {
      case ExternalOrderArised(externalOrderName, orderId, arguments) =>
        onExternalOrderArised(externalOrderName, orderId, arguments)

      case ExternalOrderVanished(externalOrderName) =>
        onExternalOrderVanished(externalOrderName)
    }

  private def onExternalOrderArised(externalOrderName: ExternalOrderName, orderId: OrderId, arguments: NamedValues)
  : Checked[OrderWatchState] =
    externalToState.get(externalOrderName) match {
      case None =>
        Right(copy(
          externalToState = externalToState + (externalOrderName -> Arised(orderId, arguments)),
          arisedQueue = arisedQueue + externalOrderName))

      case Some(HasOrder(orderId, None | Some(Vanished | VanishedAck))) =>
        Right(copy(
          externalToState = externalToState +
            (externalOrderName -> HasOrder(orderId, Some(Arised(orderId, arguments))))))

      case Some(state @ (Arised(_, _) | HasOrder(_, Some(Arised(_, _))))) =>
        Left(Problem(s"Duplicate ExternalOrderArised($externalOrderName, $arguments): $state"))
    }

  private def onExternalOrderVanished(externalOrderName: ExternalOrderName): Checked[OrderWatchState] =
    externalToState.checked(externalOrderName) flatMap {
      case Arised(_, _) =>
        Right(copy(
          externalToState = externalToState - externalOrderName,
          arisedQueue = arisedQueue - externalOrderName))

      case HasOrder(orderId, None) =>
        Right(copy(
          externalToState = externalToState + (externalOrderName -> HasOrder(orderId, Some(Vanished))),
          vanishedQueue = vanishedQueue + externalOrderName))

      case HasOrder(orderId, Some(Arised(_, _))) =>
        Right(copy(
          externalToState = externalToState + (externalOrderName -> HasOrder(orderId, None)),
          arisedQueue = arisedQueue - externalOrderName))

      case state @ HasOrder(_, Some(Vanished | VanishedAck)) =>
        Left(Problem(s"Duplicate ExternalOrderVanished($externalOrderName), state=$state"))
    }

  def onOrderAdded(externalOrderName: ExternalOrderName, orderId: OrderId): Checked[OrderWatchState] =
    externalToState.checked(externalOrderName) flatMap {
      case Arised(`orderId`, _) =>
        Right(copy(
          externalToState = externalToState + (externalOrderName -> HasOrder(orderId)),
          arisedQueue = arisedQueue - externalOrderName))

      case _ =>
        Left(Problem(s"$orderId <-: OrderAdded($externalOrderName) but not Arised($orderId)"))
    }

  def applyOrderEvent(externalOrderName: ExternalOrderName, keyedEvent: KeyedEvent[OrderCoreEvent]) = {
    import keyedEvent.{event, key => orderId}
    event match {
      case OrderRemoveMarked => onOrderRemoveMarked(externalOrderName, orderId)
      case OrderRemoved => onOrderRemoved(externalOrderName, orderId)
      case _ => Right(this)
    }
  }

  def onOrderRemoveMarked(externalOrderName: ExternalOrderName, orderId: OrderId): Checked[OrderWatchState] =
    externalToState.checked(externalOrderName) flatMap {
      case HasOrder(_, Some(Vanished) | None/*Not vanished but RemoveOrdersWhenTerminated command*/) =>
        Right(copy(
          externalToState = externalToState + (externalOrderName -> HasOrder(orderId, Some(VanishedAck))),
          vanishedQueue = vanishedQueue - externalOrderName))

      case HasOrder(_, _) =>
        logger.debug(s"$orderId <-: OrderRemoveMarked ($externalOrderName) but watched order has not vanished")
        Right(this)

      case _ =>
        Left(Problem(s"$orderId <-: OrderRemoveMarked ($externalOrderName) but not HasOrder"))
    }

  private def onOrderRemoved(externalOrderName: ExternalOrderName, orderId: OrderId): Checked[OrderWatchState] =
    externalToState.checked(externalOrderName) flatMap {
      case HasOrder(`orderId`, Some(arised: Arised)) =>
        Right(copy(
          externalToState = externalToState + (externalOrderName -> arised),
          arisedQueue = arisedQueue + externalOrderName))

      case HasOrder(`orderId`, None/*?*/ | Some(Vanished/*?*/ | VanishedAck)) =>
        Right(copy(
          externalToState = externalToState - externalOrderName))

      case state =>
        Left(Problem(s"onOrderRemoved($externalOrderName, $orderId) but state=$state"))
    }

  def nextEvents(workflowPathToVersionId: WorkflowPath => Option[VersionId])
  : Seq[KeyedEvent[OrderCoreEvent]] =
    (nextOrderAddedEvents(workflowPathToVersionId) ++ nextOrderRemovedMarkedEvents)
      .toVector

  private def nextOrderAddedEvents(workflowPathToVersionId: WorkflowPath => Option[VersionId])
  : View[KeyedEvent[OrderAdded]] =
    arisedQueue.view
      .flatMap(externalOrderName => externalToState
        .get(externalOrderName)
        .flatMap {
          case Arised(orderId, arguments) =>
            workflowPathToVersionId(orderWatch.workflowPath).map { v =>
              val workflowId = orderWatch.workflowPath ~ v
              orderId <-: OrderAdded(workflowId, arguments,
                  externalOrderKey = Some(ExternalOrderKey(id, externalOrderName)))
            }

          case _ => None
        })

  private def nextOrderRemovedMarkedEvents: View[KeyedEvent[OrderRemoveMarked]] =
    vanishedQueue.view
      .flatMap(externalOrderName => externalToState
        .get(externalOrderName)
        .collect {
          case HasOrder(orderId, _) => orderId <-: OrderRemoveMarked  // may be filtered later
        })

  def estimatedSnapshotSize =
    1 + externalToState.size

  def toSnapshot: Observable[Snapshot] =
    HeaderSnapshot(orderWatch, agentIdToAttachedState, delete) +:
      Observable.fromIterable(externalToState)
        .map { case (externalOrderName, state) =>
          ExternalOrderSnapshot(orderWatch.id, externalOrderName, state)
        }

  def applySnapshot(snapshot: ExternalOrderSnapshot): Checked[OrderWatchState] =
    externalToState.insert(snapshot.externalOrderName -> snapshot.state)
      .map(o => copy(externalToState = o))

  def onEndOfRecovery: Checked[OrderWatchState] =
    Right(recoverQueues)
}

object OrderWatchState
{
  private val logger = scribe.Logger[this.type]

  def apply(orderWatch: OrderWatch): OrderWatchState =
    OrderWatchState(orderWatch, Map.empty, false, Map.empty, Set.empty, Set.empty)

  def apply(
    orderWatch: OrderWatch,
    agentIdToAttachedState: Map[AgentPath, ItemAttachedState.NotDetached],
    sourceToOrderId: Map[ExternalOrderName, ArisedOrHasOrder])
  : OrderWatchState =
    OrderWatchState(orderWatch, agentIdToAttachedState, delete = false,
      sourceToOrderId, Set.empty, Set.empty
    ).recoverQueues

  def fromSnapshot(snapshot: HeaderSnapshot) =
    OrderWatchState(snapshot.orderWatch, snapshot.agentIdToAttachedState, snapshot.delete,
      Map.empty, Set.empty, Set.empty
    ).recoverQueues

  sealed trait Snapshot {
    def orderWatchPath: OrderWatchPath
  }

  final case class HeaderSnapshot(
    orderWatch: OrderWatch,
    agentIdToAttachedState: Map[AgentPath, ItemAttachedState.NotDetached],
    delete: Boolean)
  extends Snapshot {
    def orderWatchPath = orderWatch.id
  }

  final case class ExternalOrderSnapshot(
    orderWatchPath: OrderWatchPath,
    externalOrderName: ExternalOrderName,
    state: ArisedOrHasOrder)
  extends Snapshot

  sealed trait ArisedOrHasOrder
  sealed trait VanishedOrArised

  final case class Arised(orderId: OrderId, arguments: NamedValues)
  extends ArisedOrHasOrder with VanishedOrArised

  final case class HasOrder(
    orderId: OrderId,
    queued: Option[VanishedOrArised] = None)
  extends ArisedOrHasOrder

  final case object Vanished
  extends VanishedOrArised

  final case object VanishedAck
  extends VanishedOrArised

  object VanishedOrArised {
    private[OrderWatchState] implicit val jsonCodec = TypedJsonCodec[VanishedOrArised](
      Subtype(Vanished),
      Subtype(VanishedAck),
      Subtype(deriveCodec[Arised]))
  }

  object ArisedOrHasOrder {
    private[OrderWatchState] implicit val jsonCodec = TypedJsonCodec[ArisedOrHasOrder](
      Subtype(deriveCodec[Arised]),
      Subtype(deriveCodec[HasOrder]))
  }

  object Snapshot {
    implicit val jsonCodec = TypedJsonCodec[Snapshot](
      Subtype.named(deriveCodec[HeaderSnapshot], "OrderWatchState.Header"),
      Subtype.named(deriveCodec[ExternalOrderSnapshot], "ExternalOrder"))
  }

  intelliJuseImport(OrderAdded)
}

package js7.data.ordersource

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.AttachedState
import js7.data.event.KeyedEvent
import js7.data.item.{SimpleItemState, VersionId}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderRemoveMarked, OrderRemoved}
import js7.data.order.OrderId
import js7.data.ordersource.OrderSourceEvent.{OrderSourceOrderArised, OrderSourceOrderVanished}
import js7.data.ordersource.OrderSourceState._
import js7.data.value.NamedValues
import js7.data.workflow.WorkflowPath
import monix.reactive.Observable
import scala.collection.View

final case class OrderSourceState(
  orderSource: OrderSource,
  attached: Option[AttachedState],
  sourceOrderToState: Map[SourceOrderName, ArisedOrHasOrder],
  arisedQueue: Set[SourceOrderName],
  vanishedQueue: Set[SourceOrderName])
extends SimpleItemState
{
  def item = orderSource

  def id: OrderSourceId = orderSource.id

  type Revision = Int

  private def recoverQueues: OrderSourceState =
    copy(
      arisedQueue = sourceOrderToState.view
        .collect {
          case (orderSourceName, Arised(_)) => orderSourceName
        }
        .toSet,
      vanishedQueue = sourceOrderToState.view
        .collect {
          case (orderSourceName, HasOrder(_, Some(Vanished))) => orderSourceName
        }
        .toSet)

  def applyOrderSourceEvent(event: OrderSourceEvent): Checked[OrderSourceState] =
    event match {
      case OrderSourceOrderArised(sourceOrderName, arguments) =>
        onSourceOrderArised(sourceOrderName, arguments)

      case OrderSourceOrderVanished(sourceOrderName) =>
        onSourceOrderVanished(sourceOrderName)
    }

  private def onSourceOrderArised(sourceOrderName: SourceOrderName, arguments: NamedValues)
  : Checked[OrderSourceState] =
    sourceOrderToState.get(sourceOrderName) match {
      case Some(Arised(_) | HasOrder(_, None | Some(Arised(_)))) =>
        Left(Problem(s"Duplicate onSourceOrderArised($sourceOrderName, $arguments)"))

      case Some(HasOrder(_, Some(Vanished))) =>
        Left(Problem(s"onSourceOrderArised($sourceOrderName, $arguments) before OrderRemoveMarked"))

      case None =>
        Right(copy(
          sourceOrderToState = sourceOrderToState + (sourceOrderName -> Arised(arguments)),
          arisedQueue = arisedQueue + sourceOrderName))

      case Some(HasOrder(orderId, Some(VanishedAck))) =>
        Right(copy(
          sourceOrderToState = sourceOrderToState +
            (sourceOrderName -> HasOrder(orderId, Some(Arised(arguments))))))
    }

  private def onSourceOrderVanished(sourceOrderName: SourceOrderName): Checked[OrderSourceState] =
    sourceOrderToState.checked(sourceOrderName) flatMap {
      case Arised(_) =>
        Right(copy(
          sourceOrderToState = sourceOrderToState - sourceOrderName,
          arisedQueue = arisedQueue - sourceOrderName))

      case HasOrder(orderId, None) =>
        Right(copy(
          sourceOrderToState = sourceOrderToState +
            (sourceOrderName -> HasOrder(orderId, Some(Vanished))),
          vanishedQueue = vanishedQueue + sourceOrderName))

      case HasOrder(_, Some(Vanished)) =>
        Left(Problem(s"onSourceOrderVanished($sourceOrderName) but not Arised"))

      case state @ HasOrder(_, Some(VanishedAck)) =>
        logger.debug(s"SourceOrderVanished($sourceOrderName) but state=$state")
        Right(this)

      case HasOrder(orderId, Some(Arised(_))) =>
        Right(copy(
          sourceOrderToState = sourceOrderToState +
            (sourceOrderName -> HasOrder(orderId, None)),
          arisedQueue = arisedQueue - sourceOrderName,
          vanishedQueue = vanishedQueue - sourceOrderName))
    }

  def onOrderAdded(sourceOrderName: SourceOrderName, orderId: OrderId): Checked[OrderSourceState] =
    sourceOrderToState.checked(sourceOrderName) flatMap {
      case Arised(_) =>
        Right(copy(
          sourceOrderToState = sourceOrderToState + (sourceOrderName -> HasOrder(orderId)),
          arisedQueue = arisedQueue - sourceOrderName))

      case _ =>
        Left(Problem(s"$orderId <-: OrderAdded($sourceOrderName) but not Arised"))
    }

  def applyOrderEvent(sourceOrderName: SourceOrderName, keyedEvent: KeyedEvent[OrderCoreEvent]) = {
    import keyedEvent.{event, key => orderId}
    event match {
      case OrderRemoveMarked => onOrderRemoveMarked(sourceOrderName, orderId)
      case OrderRemoved => onOrderRemoved(sourceOrderName, orderId)
      case _ => Right(this)
    }
  }

  def onOrderRemoveMarked(sourceOrderName: SourceOrderName, orderId: OrderId): Checked[OrderSourceState] =
    sourceOrderToState.checked(sourceOrderName) flatMap {
      case HasOrder(_, Some(Vanished) | None/*Not vanished but RemoveOrdersWhenTerminated command*/) =>
        Right(copy(
          sourceOrderToState = sourceOrderToState + (sourceOrderName -> HasOrder(orderId, Some(VanishedAck))),
          vanishedQueue = vanishedQueue - sourceOrderName))

      case HasOrder(_, _) =>
        Left(Problem(s"$orderId <-: OrderRemoveMarked ($sourceOrderName) but watched order has not vanished"))

      case _ =>
        Left(Problem(s"$orderId <-: OrderRemoveMarked ($sourceOrderName) but not HasOrder"))
    }

  private def onOrderRemoved(sourceOrderName: SourceOrderName, orderId: OrderId): Checked[OrderSourceState] =
    sourceOrderToState.checked(sourceOrderName) flatMap {
      case HasOrder(`orderId`, Some(arised: Arised)) =>
        Right(copy(
          sourceOrderToState = sourceOrderToState + (sourceOrderName -> arised),
          arisedQueue = arisedQueue + sourceOrderName))

      case HasOrder(`orderId`, None/*?*/ | Some(Vanished/*?*/ | VanishedAck)) =>
        Right(copy(
          sourceOrderToState = sourceOrderToState - sourceOrderName))

      case _ =>
        Left(Problem(s"onOrderRemoved($sourceOrderName, $orderId) but not HasOder"))
    }

  def nextEvents(workflowPathToVersionId: WorkflowPath => Option[VersionId])
  : Seq[KeyedEvent[OrderCoreEvent]] =
    (nextOrderAddedEvents(workflowPathToVersionId) ++ nextOrderRemovedMarkedEvents)
      .toVector

  private def nextOrderAddedEvents(workflowPathToVersionId: WorkflowPath => Option[VersionId])
  : View[KeyedEvent[OrderAdded]] =
    arisedQueue.view
      .flatMap(sourceOrderName => sourceOrderToState
        .get(sourceOrderName)
        .flatMap {
          case Arised(arguments) =>
            workflowPathToVersionId(orderSource.workflowPath).flatMap { v =>
              val workflowId = orderSource.workflowPath ~ v
              val checked = for (orderId <- orderSource.generateOrderId(sourceOrderName)) yield
                orderId <-: OrderAdded(workflowId, arguments,
                  sourceOrderKey = Some(SourceOrderKey(id, sourceOrderName)))
              for (problem <- checked.left) logger.error(
                s"${orderSource.id} $sourceOrderName: $problem")
              checked.toOption
            }

          case _ => None
        })

  private def nextOrderRemovedMarkedEvents: View[KeyedEvent[OrderRemoveMarked]] =
    vanishedQueue.view
      .flatMap(sourceOrderName => sourceOrderToState
        .get(sourceOrderName)
        .flatMap {
          case HasOrder(orderId, _) =>
            Some(orderId <-: OrderRemoveMarked)

          case _ => None
        })

  def estimatedSnapshotSize =
    1 + sourceOrderToState.size

  def toSnapshot: Observable[Snapshot] =
    HeaderSnapshot(orderSource, attached) +:
      Observable.fromIterable(sourceOrderToState)
        .map { case (sourceOrderName, state) =>
          SourceOrderSnapshot(orderSource.id, sourceOrderName, state)
        }

  def applySnapshot(snapshot: SourceOrderSnapshot): Checked[OrderSourceState] =
    sourceOrderToState.insert(snapshot.sourceOrderName -> snapshot.state)
      .map(o => copy(sourceOrderToState = o))

  def onEndOfRecovery: Checked[OrderSourceState] =
    Right(recoverQueues)
}

object OrderSourceState
{
  private val logger = scribe.Logger[this.type]

  def apply(orderSource: OrderSource): OrderSourceState =
    OrderSourceState(orderSource, None, Map.empty, Set.empty, Set.empty)

  def apply(
    orderSource: OrderSource,
    attached: Option[AttachedState],
    sourceToOrderId: Map[SourceOrderName, ArisedOrHasOrder])
  : OrderSourceState =
    OrderSourceState(orderSource, attached, sourceToOrderId, Set.empty, Set.empty)
      .recoverQueues

  def fromSnapshot(snapshot: HeaderSnapshot) =
    OrderSourceState(snapshot.orderSource, snapshot.attached, Map.empty, Set.empty, Set.empty)

  sealed trait Snapshot {
    def orderSourceId: OrderSourceId
  }

  final case class HeaderSnapshot(
    orderSource: OrderSource,
    attached: Option[AttachedState] = None)
  extends Snapshot {
    def orderSourceId = orderSource.id
  }

  final case class SourceOrderSnapshot(
    orderSourceId: OrderSourceId,
    sourceOrderName: SourceOrderName,
    state: ArisedOrHasOrder)
  extends Snapshot

  sealed trait ArisedOrHasOrder
  sealed trait VanishedOrArised

  final case class Arised(arguments: NamedValues)
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
    private[OrderSourceState] implicit val jsonCodec = TypedJsonCodec[VanishedOrArised](
      Subtype(Vanished),
      Subtype(VanishedAck),
      Subtype(deriveCodec[Arised]))
  }

  object ArisedOrHasOrder {
    private[OrderSourceState] implicit val jsonCodec = TypedJsonCodec[ArisedOrHasOrder](
      Subtype(deriveCodec[Arised]),
      Subtype(deriveCodec[HasOrder]))
  }

  object Snapshot {
    implicit val jsonCodec = TypedJsonCodec[Snapshot](
      Subtype.named(deriveCodec[HeaderSnapshot], "OrderSourceState.Header"),
      Subtype.named(deriveCodec[SourceOrderSnapshot], "OrderSourceState.SourceOrder"))
  }

  intelliJuseImport(OrderAdded)
}

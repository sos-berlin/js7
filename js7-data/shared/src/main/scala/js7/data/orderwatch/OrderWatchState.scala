package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.{ItemAttachedState, UnsignedSimpleItemState}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderDeleted, OrderDeletionMarked}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState._
import js7.data.value.NamedValues
import monix.reactive.Observable
import scala.collection.View

final case class OrderWatchState(
  orderWatch: OrderWatch,
  externalToState: Map[ExternalOrderName, ArisedOrHasOrder],
  private[orderwatch] val arisedQueue: Set[ExternalOrderName],
  private[orderwatch] val vanishedQueue: Set[ExternalOrderName])
extends UnsignedSimpleItemState
{
  type Item = OrderWatch
  def item = orderWatch

  def id: OrderWatchPath = orderWatch.key

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
      case OrderDeletionMarked => onOrderDeletionMarked(externalOrderName, orderId)
      case OrderDeleted => onOrderDeleted(externalOrderName, orderId)
      case _ => Right(this)
    }
  }

  def onOrderDeletionMarked(externalOrderName: ExternalOrderName, orderId: OrderId): Checked[OrderWatchState] =
    externalToState.checked(externalOrderName) flatMap {
      case HasOrder(_, Some(Vanished) | None/*Not vanished but DeleteOrdersWhenTerminated command*/) =>
        Right(copy(
          externalToState = externalToState + (externalOrderName -> HasOrder(orderId, Some(VanishedAck))),
          vanishedQueue = vanishedQueue - externalOrderName))

      case HasOrder(_, _) =>
        // Happens when the file has re-arisen, leading to a second order
        //logger.debug(s"$orderId <-: OrderDeletionMarked ($externalOrderName) but watched order has not vanished")
        Right(this)

      case _ =>
        Left(Problem(s"$orderId <-: OrderDeletionMarked ($externalOrderName) but not HasOrder"))
    }

  private def onOrderDeleted(externalOrderName: ExternalOrderName, orderId: OrderId): Checked[OrderWatchState] =
    externalToState.checked(externalOrderName) flatMap {
      case HasOrder(`orderId`, Some(arised: Arised)) =>
        Right(copy(
          externalToState = externalToState + (externalOrderName -> arised),
          arisedQueue = arisedQueue + externalOrderName))

      case HasOrder(`orderId`, None/*?*/ | Some(Vanished/*?*/ | VanishedAck)) =>
        Right(copy(
          externalToState = externalToState - externalOrderName))

      case state =>
        Left(Problem(s"onOrderDeleted($externalOrderName, $orderId) but state=$state"))
    }

  def nextEvents(toOrderAdded: ToOrderAdded)
  : Seq[KeyedEvent[OrderCoreEvent]] =
    (nextOrderAddedEvents(toOrderAdded) ++ nextOrderDeletionMarkedEvents)
      .toVector

  private def nextOrderAddedEvents(toOrderAdded: ToOrderAdded)
  : View[KeyedEvent[OrderAdded]] =
    arisedQueue.view
      .flatMap(externalOrderName => externalToState
        .get(externalOrderName)
        .flatMap { case Arised(orderId, arguments) =>
          val freshOrder = FreshOrder(orderId, orderWatch.workflowPath, arguments)
          val externalOrderKey = ExternalOrderKey(id, externalOrderName)
          toOrderAdded(freshOrder, Some(externalOrderKey)) match {
            case Left(problem) =>
              // Handle this error ???
              logger.error(s"$externalOrderKey: $problem")
              None
            case Right(None) =>
              // Handle duplicate OrderId as error ???
              logger.error(s"$externalOrderKey: Duplicate OrderId ?")
              None
            case Right(Some(added)) =>
              Some(added)
          }
        })

  private def nextOrderDeletionMarkedEvents: View[KeyedEvent[OrderDeletionMarked]] =
    vanishedQueue.view
      .flatMap(externalOrderName => externalToState
        .get(externalOrderName)
        .collect {
          case HasOrder(orderId, _) => orderId <-: OrderDeletionMarked  // may be filtered later
        })

  def estimatedSnapshotSize =
    1 + externalToState.size

  def toSnapshot: Observable[Any] =
    UnsignedSimpleItemAdded(orderWatch) +:
      Observable.fromIterable(externalToState)
        .map { case (externalOrderName, state) =>
          ExternalOrderSnapshot(orderWatch.key, externalOrderName, state)
        }

  def applySnapshot(snapshot: ExternalOrderSnapshot): Checked[OrderWatchState] =
    externalToState.insert(snapshot.externalOrderName -> snapshot.state)
      .map(o => copy(externalToState = o))

  def onEndOfRecovery: Checked[OrderWatchState] =
    Right(recoverQueues)
}

object OrderWatchState
{
  type ToOrderAdded = (FreshOrder, Option[ExternalOrderKey]) => Checked[Option[KeyedEvent[OrderAdded]]]

  private val logger = scribe.Logger[this.type]

  def apply(orderWatch: OrderWatch): OrderWatchState =
    OrderWatchState(orderWatch, Map.empty, Set.empty, Set.empty)

  def apply(
    orderWatch: OrderWatch,
    agentPathToAttachedState: Map[AgentPath, ItemAttachedState.NotDetached],
    sourceToOrderId: Map[ExternalOrderName, ArisedOrHasOrder])
  : OrderWatchState =
    OrderWatchState(orderWatch, sourceToOrderId, Set.empty, Set.empty)
      .recoverQueues

  def fromSnapshot(snapshot: OrderWatchState) =
    OrderWatchState(snapshot.orderWatch, Map.empty, Set.empty, Set.empty)
      .recoverQueues

  sealed trait Snapshot {
    def orderWatchPath: OrderWatchPath
  }

  final case class HeaderSnapshot(orderWatch: OrderWatch)
  extends Snapshot {
    def orderWatchPath = orderWatch.key
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
      Subtype.named(deriveCodec[ExternalOrderSnapshot], "ExternalOrder"))
  }

  intelliJuseImport(OrderAdded)
}

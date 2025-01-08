package js7.data.orderwatch

import cats.effect.IO
import fs2.Stream
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.fs2utils.StreamExtensions.*
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.base.utils.Tests.isStrict
import js7.data.event.{EventDriven, KeyedEvent}
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.UnsignedSimpleItemState
import js7.data.order.OrderEvent.{OrderAddedEvent, OrderAddedEvents, OrderExternalVanished}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState.{Arised, ArisedOrHasOrder, ExternalOrderSnapshot, HasOrder, ToOrderAdded, Vanished, logger}
import js7.data.value.NamedValues
import scala.collection.View

/**
 * Existence of "external orders" (like files) and their coupling to JS7 Orders.
 *
 * `orderAddedQueue` contains the arisen external orders (files),
 * which are not yet tied to an JS7 Order.
 * `nextEvents` returns `OrderAdded`.
 *
 * `orderExternalVanishedQueue` contains the vanished external orders (files),
 * which are tied to an JS7 Order.
 * `nextEvents` returns `OrderDeletionMarked`.
 *
 * For `orderAddedQueue` and `orderExternalVanishedQueue` see `recalculateQueues`.
 */
final case class OrderWatchState(
  orderWatch: OrderWatch,
  externalToState: Map[ExternalOrderName, ArisedOrHasOrder] = Map.empty,
  private[orderwatch] val orderAddedQueue: Set[ExternalOrderName] = Set.empty,
  private[orderwatch] val orderExternalVanishedQueue: Set[ExternalOrderName] = Set.empty)
extends
  UnsignedSimpleItemState with EventDriven[OrderWatchState, OrderWatchEvent]:

  protected type Self = OrderWatchState
  val companion: OrderWatchState.type = OrderWatchState

  val item: OrderWatch = orderWatch
  def path: OrderWatchPath = item.path

  def updateItem(item: OrderWatch): Checked[OrderWatchState] =
    Right(copy(orderWatch = item))

  def id: OrderWatchPath = orderWatch.key

  def finishRecovery(isNotVanished: OrderId => Boolean): Checked[OrderWatchState] =
    Right(recalculateQueues(isNotVanished))

  private def recalculateQueues(isNotVanished: OrderId => Boolean): OrderWatchState =
    copy(
      orderAddedQueue =
        externalToState.view.collect:
          case (externalOrderName, Arised(_, _)) => externalOrderName
        .toSet,
      orderExternalVanishedQueue =
        externalToState.view.collect:
          case (externalOrderName, HasOrder(orderId, Some(_))) if isNotVanished(orderId) =>
            externalOrderName
        .toSet)

  def applyEvent(event: OrderWatchEvent): Checked[OrderWatchState] =
    event match
      case ExternalOrderArised(externalOrderName, orderId, arguments) =>
        onExternalOrderArised(externalOrderName, orderId, arguments)

      case ExternalOrderVanished(externalOrderName) =>
        onExternalOrderVanished(externalOrderName)

  private def onExternalOrderArised(
    externalOrderName: ExternalOrderName,
    orderId: OrderId,
    arguments: NamedValues)
  : Checked[OrderWatchState] =
    externalToState.get(externalOrderName) match
      case None =>
        Right(copy(
          externalToState = externalToState.updated(externalOrderName,
            Arised(orderId, arguments)),
          orderAddedQueue = orderAddedQueue + externalOrderName))

      case Some(HasOrder(orderId, Some(Vanished))) =>
        // Queue for an additional Order (with same OrderId)
        Right(copy(
          externalToState = externalToState.updated(externalOrderName,
            HasOrder(orderId, Some(Arised(orderId, arguments))))))

      case Some(state @ (Arised(_, _) | HasOrder(_, None | Some(Arised(_, _))))) =>
        unexpected(s"Duplicate ExternalOrderArised($externalOrderName, $arguments): $state")

  private def onExternalOrderVanished(externalOrderName: ExternalOrderName)
  : Checked[OrderWatchState] =
    externalToState.get(externalOrderName) match
      case None =>
        unexpected(s"${orderWatch.path}: Ignored ExternalOrderVanished($externalOrderName) event for unknown name")

      case Some(Arised(_, _)) =>
        // Vanished before Order has been added
        Right(copy(
          externalToState = externalToState - externalOrderName,
          orderAddedQueue = orderAddedQueue - externalOrderName))

      case Some(HasOrder(orderId, None)) =>
        // Vanished after Order has been added – the normal case
        Right(copy(
          externalToState = externalToState.updated(externalOrderName,
            HasOrder(orderId, Some(Vanished))),
          orderExternalVanishedQueue = orderExternalVanishedQueue + externalOrderName))

      case Some(HasOrder(orderId, Some(Arised(_, _)))) =>
        // The re-arisen external order (file) has vanished again – we ignore it
        Right(copy(
          externalToState = externalToState.updated(externalOrderName,
            HasOrder(orderId, Some(Vanished)))))

      case Some(state @ HasOrder(_, Some(Vanished))) =>
        unexpected(s"Duplicate ExternalOrderVanished($externalOrderName), state=$state")

  def onOrderAdded(externalOrderName: ExternalOrderName, orderId: OrderId)
  : Checked[OrderWatchState] =
    externalToState.checked(externalOrderName) flatMap:
      case Arised(`orderId`, _) =>
        Right(copy(
          externalToState = externalToState.updated(externalOrderName, HasOrder(orderId)),
          orderAddedQueue = orderAddedQueue - externalOrderName))

      case _ =>
        unexpected(s"$orderId <-: OrderAdded($externalOrderName) but not Arised($orderId)")

  def onOrderExternalVanished(externalOrderName: ExternalOrderName): Checked[OrderWatchState] =
    Right(copy(
      orderExternalVanishedQueue = orderExternalVanishedQueue - externalOrderName))

  def onOrderDeleted(externalOrderName: ExternalOrderName, orderId: OrderId)
  : Checked[OrderWatchState] =
    externalToState.get(externalOrderName) match
      case Some(HasOrder(`orderId`, None/*?*/ | Some(Vanished))) | None =>
        Right(copy(
          externalToState = externalToState - externalOrderName,
          orderExternalVanishedQueue = orderExternalVanishedQueue - externalOrderName))

      case Some(HasOrder(`orderId`, Some(queued: Arised))) =>
        // The re-arisen ExternalOrderName has been deleted.
        // We insert the ExternalOrderName into orderAddedQueue to start a new Order.
        Right(copy(
          externalToState = externalToState.updated(externalOrderName, queued),
          orderExternalVanishedQueue = orderExternalVanishedQueue - externalOrderName,
          orderAddedQueue = orderAddedQueue + externalOrderName))

      case Some(Arised(_, _)) =>
        // The re-arisen ExternalOrderName has been deleted.
        // We insert the ExternalOrderName into orderAddedQueue to start a new Order.
        Right(copy(
          orderExternalVanishedQueue = orderExternalVanishedQueue - externalOrderName,
          orderAddedQueue = orderAddedQueue + externalOrderName))

      case Some(x) =>
        unexpected(s"${orderWatch.path}: unexpected $orderId <-: OrderDeleted for $x")

  def nextEvents(toOrderAdded: ToOrderAdded)
  : View[KeyedEvent[OrderAddedEvent | OrderExternalVanished]] =
    nextOrderExternalVanishedEvents ++ nextOrderAddedEvents(toOrderAdded)

  private def nextOrderAddedEvents(toOrderAdded: ToOrderAdded)
  : View[KeyedEvent[OrderAddedEvent]] =
    orderAddedQueue.view.flatMap: externalOrderName =>
      externalToState.get(externalOrderName)
        .toList.flatMap: arised =>
          val Arised(orderId, arguments) = arised: @unchecked
          val freshOrder = FreshOrder(orderId, orderWatch.workflowPath, arguments)
          val externalOrderKey = ExternalOrderKey(id, externalOrderName)
          toOrderAdded(freshOrder, Some(externalOrderKey)) match
            case Left(problem) =>
              // Happens when the Order's Plan is closed
              logger.error(s"${orderWatch.path}: $externalOrderKey: $problem")
              Nil

            case Right(Left(existingOrder)) =>
              val vanished = existingOrder.externalOrder
                .filter(_.externalOrderKey == externalOrderKey)
                .exists(_.vanished)
              if !vanished then
                logger.error(s"${orderWatch.path}: ${freshOrder.id} already exists${
                  existingOrder.externalOrder.fold("")(o => s" and is linked to $o")}")
              Nil

            case Right(Right(orderAddedEvents)) =>
              orderAddedEvents.toKeyedEvents

  private def nextOrderExternalVanishedEvents: View[KeyedEvent[OrderExternalVanished]] =
    orderExternalVanishedQueue.view.flatMap: externalOrderName =>
      externalToState.get(externalOrderName).collect:
        case HasOrder(orderId, _) => orderId <-: OrderExternalVanished

  def estimatedSnapshotSize: Int =
    1 + externalToState.size

  override def toSnapshotStream: Stream[IO, Any] =
    UnsignedSimpleItemAdded(orderWatch) +:
      Stream.iterable(externalToState)
        .map: (externalOrderName, state) =>
          ExternalOrderSnapshot(orderWatch.key, externalOrderName, state)

  def applySnapshot(snapshot: ExternalOrderSnapshot): Checked[OrderWatchState] =
    externalToState.insert(snapshot.externalOrderName -> snapshot.state)
      .map(o => copy(externalToState = o))

  private def unexpected(msg: String): Checked[this.type] =
    if isStrict then
      Left(Problem(msg))
    else
      logger.error(msg)
      Right(this)

  override def toString =
    s"OrderWatchState(${orderWatch.path} $externalToState orderAddedQueue=${
      orderAddedQueue.mkString("{", " ", "}")} orderExternalVanishedQueue=${
      orderExternalVanishedQueue.mkString("{", " ", "}")})"


object OrderWatchState
extends UnsignedSimpleItemState.Companion[OrderWatchState]
with EventDriven.Companion[OrderWatchState, OrderWatchEvent]:

  type Key = OrderWatchPath
  type Item = OrderWatch
  override type ItemState = OrderWatchState

  type ToOrderAdded =
    (FreshOrder, Option[ExternalOrderKey]) =>
      Checked[Either[Order[Order.State], OrderAddedEvents]]

  private val logger = Logger[this.type]

  def apply(orderWatch: OrderWatch): OrderWatchState =
    OrderWatchState(orderWatch, Map.empty, Set.empty, Set.empty)


  sealed trait Snapshot:
    def orderWatchPath: OrderWatchPath


  final case class HeaderSnapshot(orderWatch: OrderWatch)
  extends Snapshot:
    def orderWatchPath: OrderWatchPath =
      orderWatch.key


  final case class ExternalOrderSnapshot(
    orderWatchPath: OrderWatchPath,
    externalOrderName: ExternalOrderName,
    state: ArisedOrHasOrder)
  extends Snapshot

  sealed trait ArisedOrHasOrder
  sealed trait VanishedOrArised


  final case class Arised(orderId: OrderId, arguments: NamedValues)
  extends ArisedOrHasOrder, VanishedOrArised:
    override def toString = s"Arised($orderId)"

  final case class HasOrder(
    orderId: OrderId,
    queued: Option[VanishedOrArised] = None)
  extends ArisedOrHasOrder

  case object Vanished
  extends VanishedOrArised

  object VanishedOrArised:
    private[orderwatch] implicit val jsonCodec: TypedJsonCodec[VanishedOrArised] = TypedJsonCodec(
      Subtype.singleton(Vanished, aliases = Seq("VanishedAck")/*COMPATIBLE with v2.2.1*/),
      Subtype(deriveCodec[Arised]))

  object ArisedOrHasOrder:
    private[orderwatch] implicit val jsonCodec: TypedJsonCodec[ArisedOrHasOrder] = TypedJsonCodec(
      Subtype(deriveCodec[Arised]),
      Subtype(deriveCodec[HasOrder]))

  object Snapshot:
    implicit val jsonCodec: TypedJsonCodec[Snapshot] = TypedJsonCodec(
      Subtype.named(deriveCodec[ExternalOrderSnapshot], "ExternalOrder"))

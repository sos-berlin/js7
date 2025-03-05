package js7.data.orderwatch

import fs2.Stream
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.fs2utils.StreamExtensions.*
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.base.utils.Tests.isStrict
import js7.data.event.{EventDriven, KeyedEvent}
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.UnsignedSimpleItemState
import js7.data.order.OrderEvent.{OrderAddedEvent, OrderAddedEvents, OrderExternalVanished}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderAppeared, ExternalOrderRejected, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState.{Appeared, AppearedOrHasOrder, ExternalOrderSnapshot, HasOrder, Rejected, ToOrderAdded, Vanished, logger}
import js7.data.value.NamedValues
import scala.collection.View

/**
 * Existence of "external orders" (like files) and their coupling to JS7 Orders.
 *
 * `orderAddedQueue` contains the appeared external orders (files),
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
  item: OrderWatch,
  externalToState: Map[ExternalOrderName, AppearedOrHasOrder] = Map.empty,
  private[orderwatch] val orderAddedQueue: Set[ExternalOrderName] = Set.empty,
  private[orderwatch] val orderExternalVanishedQueue: Set[ExternalOrderName] = Set.empty)
extends
  UnsignedSimpleItemState with EventDriven[OrderWatchState, OrderWatchEvent]:

  protected type Self = OrderWatchState
  val companion: OrderWatchState.type = OrderWatchState

  def orderWatch: OrderWatch = item
  def path: OrderWatchPath = item.path

  def updateItem(item: OrderWatch): Checked[OrderWatchState] =
    Right(copy(item = item))

  def finishRecovery(isNotVanished: OrderId => Boolean): Checked[OrderWatchState] =
    Right(recalculateQueues(isNotVanished))

  private def recalculateQueues(isNotVanished: OrderId => Boolean): OrderWatchState =
    copy(
      orderAddedQueue =
        externalToState.view.collect:
          case (externalOrderName, _: Appeared) => externalOrderName
        .toSet,
      orderExternalVanishedQueue =
        externalToState.view.collect:
          case (externalOrderName, HasOrder(orderId, Some(_))) if isNotVanished(orderId) =>
            externalOrderName
        .toSet)

  def applyEvent(event: OrderWatchEvent): Checked[OrderWatchState] =
    event match
      case ExternalOrderAppeared(externalOrderName, arguments, legacyOrderId) =>
        onExternalOrderAppeared(externalOrderName, arguments, legacyOrderId)

      case ExternalOrderRejected(externalOrderName, problem) =>
        onExternalOrderRejected(externalOrderName, problem)

      case ExternalOrderVanished(externalOrderName) =>
        onExternalOrderVanished(externalOrderName)

  private def onExternalOrderAppeared(
    externalOrderName: ExternalOrderName,
    arguments: NamedValues,
    legacyOrderId: Option[OrderId])
  : Checked[OrderWatchState] =
    externalToState.get(externalOrderName) match
      case None =>
        Right(copy(
          externalToState = externalToState.updated(
            externalOrderName,
            Appeared(arguments, legacyOrderId)),
          orderAddedQueue = orderAddedQueue + externalOrderName))

      case Some(HasOrder(orderId, Some(Vanished))) =>
        // Queue for an additional Order (with same OrderId)
        Right(copy(
          externalToState = externalToState.updated(externalOrderName,
            HasOrder(orderId, Some(Appeared(arguments, legacyOrderId))))))

      case Some(state @ (Appeared(_, _) | Rejected(_) | HasOrder(_, None | Some(Appeared(_, _))))) =>
        unexpected:
          s"Duplicate ExternalOrderAppeared(${externalOrderName.string}, $arguments): $state"

  private def onExternalOrderRejected(externalOrderName: ExternalOrderName, problem: Problem)
  : Checked[OrderWatchState] =
    Right(copy(
      externalToState =
        if problem.is(FileWatch.FileWatchPatternDoesntMatchProblem) then
          // The Agent matched the filename, but the Controller didn't.
          // This may happen when Controller and Agent have different revisions of the FileWatch.
          // Because the filename does not match at the Agent, the Agent never will emit an
          // ExternalOrderVanished.
          // Therefore, we remove the externalOrderName.
          externalToState - externalOrderName
        else
          externalToState.updated(externalOrderName,
        Rejected(problem)),
      orderAddedQueue = orderAddedQueue - externalOrderName))

  private def onExternalOrderVanished(externalOrderName: ExternalOrderName)
  : Checked[OrderWatchState] =
    externalToState.get(externalOrderName) match
      case None =>
        unexpected:
          s"$path: Ignored ExternalOrderVanished(${externalOrderName.string}) event for unknown name"

      case Some(Appeared(_, _)) =>
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

      case Some(HasOrder(orderId, Some(Appeared(_, _)))) =>
        // The reappeared external order (file) has vanished again – we ignore it
        Right(copy(
          externalToState = externalToState.updated(externalOrderName,
            HasOrder(orderId, Some(Vanished)))))

      case Some(Rejected(_)) =>
        Right(copy(
          externalToState = externalToState - externalOrderName))

      case Some(state @ HasOrder(_, Some(Vanished))) =>
        unexpected:
          s"Duplicate ExternalOrderVanished(${externalOrderName.string}), state=$state"

  def onOrderAdded(externalOrderName: ExternalOrderName, orderId: OrderId)
  : Checked[OrderWatchState] =
    externalToState.checked(externalOrderName) flatMap:
      case Appeared(_, `orderId` | None) =>
        Right(copy(
          externalToState = externalToState.updated(externalOrderName, HasOrder(orderId)),
          orderAddedQueue = orderAddedQueue - externalOrderName))

      case o =>
        unexpected(s"$orderId <-: OrderAdded($externalOrderName) but externalToState is $o")

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

      case Some(HasOrder(`orderId`, Some(queued: Appeared))) =>
        // The reappeared ExternalOrderName has been deleted.
        // We insert the ExternalOrderName into orderAddedQueue to start a new Order.
        Right(copy(
          externalToState = externalToState.updated(externalOrderName, queued),
          orderExternalVanishedQueue = orderExternalVanishedQueue - externalOrderName,
          orderAddedQueue = orderAddedQueue + externalOrderName))

      case Some(Appeared(_, _)) =>
        // The reappeared ExternalOrderName has been deleted.
        // We insert the ExternalOrderName into orderAddedQueue to start a new Order.
        Right(copy(
          orderExternalVanishedQueue = orderExternalVanishedQueue - externalOrderName,
          orderAddedQueue = orderAddedQueue + externalOrderName))

      case Some(x) =>
        unexpected(s"$path: unexpected $orderId <-: OrderDeleted for $x")

  def nextEvents(toOrderAdded: ToOrderAdded)
  : View[KeyedEvent[OrderAddedEvent | ExternalOrderRejected | OrderExternalVanished]] =
    nextOrderExternalVanishedEvents ++ nextOrderAddedEvents(toOrderAdded)

  private def nextOrderAddedEvents(toOrderAdded: ToOrderAdded)
  : View[KeyedEvent[OrderAddedEvent | ExternalOrderRejected]] =
    orderAddedQueue.view.flatMap: externalOrderName =>
      val externalOrderKey = path / externalOrderName
      externalToState.get(externalOrderName).toList.flatMap: appearedOrHasOrder =>
        val Appeared(arguments, legacyOrderId) = appearedOrHasOrder: @unchecked
        item.externalToOrderAndPlanId(externalOrderName, legacyOrderId, Timestamp.now)
          .flatMap: (orderId, planId) =>
            val freshOrder = FreshOrder(orderId, orderWatch.workflowPath, arguments, planId)
            toOrderAdded(freshOrder, Some(externalOrderKey))
              .map(freshOrder.id -> _)
        match
          case Left(problem) =>
            // Happens when evalPlanIdExpr failed or the Order's Plan is closed
            (path <-: ExternalOrderRejected(externalOrderName, problem)) :: Nil

          case Right((newOrderId, Left(existingOrder))) =>
            val vanished = existingOrder.externalOrder
              .filter(_.externalOrderKey == externalOrderKey)
              .exists(_.vanished)
            if !vanished then
              logger.error(s"$path: $newOrderId already exists${
                existingOrder.externalOrder.fold("")(o => s" and is linked to $o")}")
            Nil

          case Right((_, Right(orderAddedEvents))) =>
            orderAddedEvents.toKeyedEvents

  private def nextOrderExternalVanishedEvents: View[KeyedEvent[OrderExternalVanished]] =
    orderExternalVanishedQueue.view.flatMap: externalOrderName =>
      externalToState.get(externalOrderName).collect:
        case HasOrder(orderId, _) => orderId <-: OrderExternalVanished

  def estimatedSnapshotSize: Int =
    1 + externalToState.size

  override def toSnapshotStream: Stream[fs2.Pure, Any] =
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
    s"OrderWatchState($path $externalToState " +
      s"orderAddedQueue=${orderAddedQueue.mkString("{", " ", "}")
      } orderExternalVanishedQueue=${orderExternalVanishedQueue.mkString("{", " ", "}")})"


object OrderWatchState
extends UnsignedSimpleItemState.Companion[OrderWatchState]
with EventDriven.Companion[OrderWatchState, OrderWatchEvent]:

  type Key = OrderWatchPath
  type Item = OrderWatch
  override type ItemState = OrderWatchState

  type ToOrderAdded =
    (FreshOrder, Option[ExternalOrderKey]) => Checked[Either[Order[Order.State], OrderAddedEvents]]

  private val logger = Logger[this.type]

  def apply(orderWatch: OrderWatch): OrderWatchState =
    OrderWatchState(orderWatch, Map.empty, Set.empty, Set.empty)


  sealed trait Snapshot:
    def orderWatchPath: OrderWatchPath

  object Snapshot:
    given TypedJsonCodec[Snapshot] = TypedJsonCodec(
      Subtype.named(deriveCodec[ExternalOrderSnapshot], "ExternalOrder"))


  final case class HeaderSnapshot(orderWatch: OrderWatch)
  extends Snapshot:
    def orderWatchPath: OrderWatchPath =
      orderWatch.key


  final case class ExternalOrderSnapshot(
    orderWatchPath: OrderWatchPath,
    externalOrderName: ExternalOrderName,
    state: AppearedOrHasOrder)
  extends Snapshot


  sealed trait AppearedOrHasOrder

  object AppearedOrHasOrder:
    private[orderwatch] given TypedJsonCodec[AppearedOrHasOrder] = TypedJsonCodec(
      Appeared.subtype,
      Subtype(deriveCodec[HasOrder]),
      Subtype(deriveCodec[Rejected]))


  sealed trait AppearedOrVanished

  private object AppearedOrVanished:
    private[orderwatch] given TypedJsonCodec[AppearedOrVanished] = TypedJsonCodec(
      Subtype.singleton(Vanished, aliases = Seq("VanishedAck") /*COMPATIBLE with v2.2.1*/),
      Appeared.subtype)


  // TODO Rename as Appeared
  final case class Appeared(arguments: NamedValues, orderId: Option[OrderId] = None)
  extends AppearedOrHasOrder, AppearedOrVanished:
    override def toString = s"Appeared${orderId.fold("")(o => s"($o)")}"

  object Appeared:
    val subtype: Subtype[Appeared] =
      Subtype(deriveCodec[Appeared], aliases = Seq("Arised"/*COMPATIBLE with v2.7.3*/))

  final case class HasOrder(
    orderId: OrderId,
    queued: Option[AppearedOrVanished] = None)
  extends AppearedOrHasOrder


  final case class Rejected(problem: Problem)
  extends AppearedOrHasOrder


  case object Vanished
  extends AppearedOrVanished

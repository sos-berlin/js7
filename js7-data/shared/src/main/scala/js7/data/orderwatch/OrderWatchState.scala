package js7.data.orderwatch

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.base.utils.Tests.isStrict
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.{ItemAttachedState, UnsignedSimpleItemState}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderDeletionMarked}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState.{Arised, ArisedOrHasOrder, ExternalOrderSnapshot, HasOrder, ToOrderAdded, Vanished, logger}
import js7.data.value.NamedValues
import js7.data.workflow.position.BranchPath.syntax.*
import monix.reactive.Observable
import scala.collection.View

/**
 * Existence of "external orders" (like files) and their coupling to JS7 Orders.
 *
 * `arisedQueue` contains the arisen external orders (files),
 * which are not yet tied to an JS7 Order.
 * `nextEvents` returns `OrderAdded`.
 *
 * `vanishedQueue` contains the vanished external orders (files),
 * which are tied to an JS7 Order.
 * `nextEvents` returns `OrderDeletionMarked`.
 *
 * For `arisedQueue` and `vanishedQueue` see `recalculateQueues`.
 */
final case class OrderWatchState(
  orderWatch: OrderWatch,
  externalToState: Map[ExternalOrderName, ArisedOrHasOrder],
  private[orderwatch] val arisedQueue: Set[ExternalOrderName],
  private[orderwatch] val vanishedQueue: Set[ExternalOrderName])
extends UnsignedSimpleItemState
{
  protected type Self = OrderWatchState
  val companion: OrderWatchState.type = OrderWatchState

  val item: OrderWatch = orderWatch
  def path: OrderWatchPath = item.path

  def updateItem(item: OrderWatch): Checked[OrderWatchState] =
    Right(copy(orderWatch = item))

  def id: OrderWatchPath = orderWatch.key

  type Revision = Int

  def finishRecovery: Checked[OrderWatchState] =
    Right(recalculateQueues)

  private def recalculateQueues: OrderWatchState =
    copy(
      arisedQueue = externalToState.view
        .collect {
          case (externalOrderName, Arised(_, _)) => externalOrderName
        }
        .toSet,
      vanishedQueue = externalToState.view
        .collect {
          case (externalOrderName, HasOrder(_, Some(_))) => externalOrderName
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

      case Some(HasOrder(orderId, Some(Vanished))) =>
        // Queue for an additional Order (with same OrderId)
        Right(copy(
          externalToState = externalToState +
            (externalOrderName -> HasOrder(orderId, Some(Arised(orderId, arguments))))))

      case Some(state @ (Arised(_, _) | HasOrder(_, None | Some(Arised(_, _))))) =>
        unexpected(s"Duplicate ExternalOrderArised($externalOrderName, $arguments): $state")
    }

  private def onExternalOrderVanished(externalOrderName: ExternalOrderName)
  : Checked[OrderWatchState] =
    externalToState.get(externalOrderName) match {
      case None =>
        unexpected(s"${orderWatch.path}: Ignored ExternalOrderVanished($externalOrderName) event for unknown name")

      case Some(Arised(_, _)) =>
        // Vanished before Order has been added
        Right(copy(
          externalToState = externalToState - externalOrderName,
          arisedQueue = arisedQueue - externalOrderName))

      case Some(HasOrder(orderId, None)) =>
        // Vanished after Order has been added – the normal case
        Right(copy(
          externalToState = externalToState + (externalOrderName -> HasOrder(orderId, Some(Vanished))),
          vanishedQueue = vanishedQueue + externalOrderName))

      case Some(HasOrder(orderId, Some(Arised(_, _)))) =>
        // The re-arisen external order (file) has vanished again – we ignore it
        Right(copy(
          externalToState = externalToState + (externalOrderName -> HasOrder(orderId, Some(Vanished)))))

      case Some(state @ HasOrder(_, Some(Vanished))) =>
        unexpected(s"Duplicate ExternalOrderVanished($externalOrderName), state=$state")
    }

  def onOrderAdded(externalOrderName: ExternalOrderName, orderId: OrderId): Checked[OrderWatchState] =
    externalToState.checked(externalOrderName) flatMap {
      case Arised(`orderId`, _) =>
        Right(copy(
          externalToState = externalToState + (externalOrderName -> HasOrder(orderId)),
          arisedQueue = arisedQueue - externalOrderName))

      case _ =>
        unexpected(s"$orderId <-: OrderAdded($externalOrderName) but not Arised($orderId)")
    }

  def onOrderDeleted(externalOrderName: ExternalOrderName, orderId: OrderId)
  : Checked[OrderWatchState] =
    externalToState.get(externalOrderName) match {
      case Some(HasOrder(`orderId`, None/*?*/ | Some(Vanished))) | None =>

        Right(copy(
          externalToState = externalToState - externalOrderName,
          vanishedQueue = vanishedQueue - externalOrderName))

      case Some(HasOrder(`orderId`, Some(arised: Arised))) =>
        // The re-arisen ExternalOrderName has been deleted.
        // We insert the ExternalOrderName into arisedQueue, to start a new Order.
        Right(copy(
          externalToState = externalToState + (externalOrderName -> arised),
          vanishedQueue = vanishedQueue - externalOrderName,
          arisedQueue = arisedQueue + externalOrderName))

      case Some(x) =>
        unexpected(s"${orderWatch.path}: unexpected onOrderDeleted($externalOrderName, $orderId) for $x")
    }

  def nextEvents(toOrderAdded: ToOrderAdded, isDeletionMarkable: OrderId => Boolean)
  : View[KeyedEvent[OrderCoreEvent]] =
    nextOrderAddedEvents(toOrderAdded) ++
      nextOrderDeletionMarkedEvents(isDeletionMarkable)

  private def nextOrderAddedEvents(toOrderAdded: ToOrderAdded)
  : View[KeyedEvent[OrderAdded]] =
    arisedQueue.view
      .flatMap(externalOrderName => externalToState
        .get(externalOrderName)
        .flatMap { arised =>
          val Arised(orderId, arguments) = arised: @unchecked
          val freshOrder = FreshOrder(orderId, orderWatch.workflowPath, arguments)
          val externalOrderKey = ExternalOrderKey(id, externalOrderName)
          toOrderAdded(freshOrder, Some(externalOrderKey)) match {
            case Left(problem) =>
              // Should not happen
              logger.error(s"${orderWatch.path}: $externalOrderKey: $problem")
              None

            case Right(None) =>
              // Should not happen
              logger.error(s"${orderWatch.path}: $externalOrderKey: Duplicate OrderId ?")
              None

            case Right(Some(added)) =>
              Some(added)
          }
        })

  private def nextOrderDeletionMarkedEvents(isDeletionMarkable: OrderId => Boolean)
  : View[KeyedEvent[OrderDeletionMarked]] =
    vanishedQueue.view
      .flatMap(externalOrderName => externalToState
        .get(externalOrderName)
        .collect { case HasOrder(orderId, _) => orderId }
        .filter(isDeletionMarkable)
        .map(_ <-: OrderDeletionMarked)
      )

  def estimatedSnapshotSize =
    1 + externalToState.size

  override def toSnapshotObservable: Observable[Any] =
    UnsignedSimpleItemAdded(orderWatch) +:
      Observable.fromIterable(externalToState)
        .map { case (externalOrderName, state) =>
          ExternalOrderSnapshot(orderWatch.key, externalOrderName, state)
        }

  def applySnapshot(snapshot: ExternalOrderSnapshot): Checked[OrderWatchState] =
    externalToState.insert(snapshot.externalOrderName -> snapshot.state)
      .map(o => copy(externalToState = o))

  private def unexpected(msg: String): Checked[this.type] =
    if isStrict then
      Left(Problem(msg))
    else {
      logger.error(msg)
      Right(this)
    }
}

object OrderWatchState extends UnsignedSimpleItemState.Companion[OrderWatchState]
{
  type Key = OrderWatchPath
  type Item = OrderWatch
  override type ItemState = OrderWatchState

  type ToOrderAdded = (FreshOrder, Option[ExternalOrderKey]) => Checked[Option[KeyedEvent[OrderAdded]]]

  private val logger = Logger[this.type]

  def apply(orderWatch: OrderWatch): OrderWatchState =
    OrderWatchState(orderWatch, Map.empty, Set.empty, Set.empty)

  def apply(
    orderWatch: OrderWatch,
    agentPathToAttachedState: Map[AgentPath, ItemAttachedState.NotDetached],
    sourceToOrderId: Map[ExternalOrderName, ArisedOrHasOrder])
  : OrderWatchState =
    OrderWatchState(orderWatch, sourceToOrderId, Set.empty, Set.empty)
      .recalculateQueues

  def fromSnapshot(snapshot: OrderWatchState) =
    OrderWatchState(snapshot.orderWatch, Map.empty, Set.empty, Set.empty)
      .recalculateQueues

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

  case object Vanished
  extends VanishedOrArised

  object VanishedOrArised {
    private[orderwatch] implicit val jsonCodec: TypedJsonCodec[VanishedOrArised] = TypedJsonCodec(
      Subtype.singleton(Vanished, aliases = Seq("VanishedAck")/*COMPATIBLE with v2.2.1*/),
      Subtype(deriveCodec[Arised]))
  }

  object ArisedOrHasOrder {
    private[orderwatch] implicit val jsonCodec: TypedJsonCodec[ArisedOrHasOrder] = TypedJsonCodec(
      Subtype(deriveCodec[Arised]),
      Subtype(deriveCodec[HasOrder]))
  }

  object Snapshot {
    implicit val jsonCodec: TypedJsonCodec[Snapshot] = TypedJsonCodec(
      Subtype.named(deriveCodec[ExternalOrderSnapshot], "ExternalOrder"))
  }

  intelliJuseImport(OrderAdded)
}

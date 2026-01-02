package js7.data.orderwatch

import cats.instances.vector.*
import cats.syntax.traverse.*
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.{OrderAddedEvent, OrderExternalVanished}
import js7.data.order.{Order, OrderId}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderRejected, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState.ToOrderAdded
import scala.collection.{MapView, View}

trait OrderWatchStateHandler[Self]:
  self: Self =>

  protected def pathToOrderWatchState: MapView[OrderWatchPath, OrderWatchState]
  protected def idToOrder: PartialFunction[OrderId, Order[Order.State]]

  protected def isOrderExternalNotVanished(orderId: OrderId): Boolean

  protected def updateOrderWatchStates(
    orderWatchStates: Seq[OrderWatchState] = Nil,
    remove: Seq[OrderWatchPath] = Nil)
  : Checked[Self]

  private def updateOrderWatchState(orderWatchState: OrderWatchState): Checked[Self] =
    updateOrderWatchStates(orderWatchState :: Nil)

  /** OrderWatchStateHandler. */
  object ow:
    def addOrderWatch(orderWatchState: OrderWatchState): Checked[Self] =
      pathToOrderWatchState.checkNoDuplicate(orderWatchState.item.key)
        .flatMap(_ => updateOrderWatchState(orderWatchState))

    def removeOrderWatch(orderWatchPath: OrderWatchPath): Checked[Self] =
      updateOrderWatchStates(remove = orderWatchPath :: Nil)

    def changeOrderWatch(changed: OrderWatch): Checked[Self] =
      pathToOrderWatchState
        .checked(changed.key)
        .flatMap: watchState =>
          updateOrderWatchState(watchState.copy(
            item = changed))

    def onOrderWatchEvent(keyedEvent: KeyedEvent[OrderWatchEvent])
    : Checked[Self] =
      pathToOrderWatchState
        .checked(keyedEvent.key)
        .flatMap(_.applyEvent(keyedEvent.event))
        .flatMap(updateOrderWatchState)

    def onOrderAdded(order: Order[Order.State]): Checked[Self] =
      order.externalOrder match
        case None => Right(self)
        case Some(ext) => onOrderAdded(ext.externalOrderKey, order.id)

    def onOrderAdded(externalOrderKey: ExternalOrderKey, orderId: OrderId): Checked[Self] =
      import externalOrderKey.orderWatchPath
      pathToOrderWatchState
        .checked(orderWatchPath)
        .flatMap(_.onOrderAdded(externalOrderKey.name, orderId))
        .flatMap(updateOrderWatchState)

    def onOrderDeleted(externalOrderKey: ExternalOrderKey, orderId: OrderId): Checked[Self] =
      import externalOrderKey.{name, orderWatchPath}
      pathToOrderWatchState.get(orderWatchPath).fold(Checked(self)): orderWatchState =>
        orderWatchState.onOrderDeleted(name, orderId)
          .flatMap(updateOrderWatchState)

    def toOrderExternalVanished(path: OrderWatchPath, vanished: ExternalOrderVanished)
    : Option[KeyedEvent[OrderExternalVanished]] =
      pathToOrderWatchState.get(path).flatMap:
        _.toOrderExternalVanished(vanished)

    def nextEvents(toOrderAdded: ToOrderAdded)
    : View[KeyedEvent[OrderAddedEvent | ExternalOrderRejected | OrderExternalVanished]] =
      pathToOrderWatchState.values.view.flatMap:
        _.nextEvents(toOrderAdded)

    def applySnapshot(snapshot: OrderWatchState.ExternalOrderSnapshot): Checked[Self] =
      pathToOrderWatchState.checked(snapshot.orderWatchPath)
        .flatMap(_
          .applySnapshot(snapshot)
          .flatMap(watchState => updateOrderWatchState(
            watchState)))

    def finishRecovery: Checked[Self] =
      pathToOrderWatchState.values
        .toVector
        .traverse:
          _.finishRecovery(isNotVanished = isOrderExternalNotVanished)
        .flatMap(updateOrderWatchStates(_))

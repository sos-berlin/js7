package js7.data.orderwatch

import cats.instances.vector.*
import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.base.utils.{Assertions, Tests}
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.{OrderAddedEvent, OrderExternalVanished}
import js7.data.order.{Order, OrderEvent, OrderId}
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
        .flatMap(watchState =>
          updateOrderWatchState(watchState.copy(
            orderWatch = changed)))

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

    def onOrderExternalVanished(order: Order[Order.State]): Checked[Self] =
      if isStrict then assertThat(order.externalOrder.exists(_.vanished))
      order.externalOrder.toRight:
        Problem(s"OrderExternalVanished but ${order.id} is not linked to an external order")
      .flatMap: ext =>
        onOrderExternalVanished(ext.externalOrderKey)

    def onOrderExternalVanished(external: ExternalOrderKey): Checked[Self] =
      pathToOrderWatchState.get(external.orderWatchPath)
        .fold(Checked(self)):
          _.onOrderExternalVanished(external.name).flatMap:
            updateOrderWatchState

    def onOrderDeleted(externalOrderKey: ExternalOrderKey, orderId: OrderId): Checked[Self] =
      import externalOrderKey.{name, orderWatchPath}
      pathToOrderWatchState.get(orderWatchPath).fold(Checked(self)): orderWatchState =>
        orderWatchState.onOrderDeleted(name, orderId)
          .flatMap(updateOrderWatchState)

    def nextEvents(toOrderAdded: ToOrderAdded): View[KeyedEvent[OrderAddedEvent | OrderExternalVanished]] =
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

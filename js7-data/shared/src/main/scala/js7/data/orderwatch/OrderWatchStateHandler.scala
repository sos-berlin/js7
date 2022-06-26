package js7.data.orderwatch

import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEvent
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.OrderWatchState.ToOrderAdded
import scala.collection.{MapView, View}

trait OrderWatchStateHandler[Self]
{
  self: Self =>

  protected def pathToOrderWatchState: MapView[OrderWatchPath, OrderWatchState]

  protected def updateOrderWatchStates(
    orderWatchStates: Iterable[OrderWatchState] = Nil,
    remove: Iterable[OrderWatchPath] = Nil)
  : Checked[Self]

  private def updateOrderWatchState(orderWatchState: OrderWatchState): Checked[Self] =
    updateOrderWatchStates(orderWatchState :: Nil)

  /** OrderWatchStateHandler. */
  object ow {
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
        .flatMap(_.applyOrderWatchEvent(keyedEvent.event))
        .flatMap(updated => updateOrderWatchState(
          updated))

    def onOrderAdded(order: Order[Order.State]): Checked[Self] =
      order.externalOrderKey match {
        case None => Right(self)
        case Some(externalOrderKey) => onOrderAdded(externalOrderKey, order.id)
      }

    private def onOrderAdded(externalOrderKey: ExternalOrderKey, orderId: OrderId): Checked[Self] = {
      import externalOrderKey.orderWatchPath
      pathToOrderWatchState
        .checked(orderWatchPath)
        .flatMap(_.onOrderAdded(externalOrderKey.name, orderId))
        .flatMap(watchState => updateOrderWatchState(
          watchState))
    }

    def onOrderDeleted(externalOrderKey: ExternalOrderKey, orderId: OrderId): Checked[Self] = {
      import externalOrderKey.{name, orderWatchPath}
      pathToOrderWatchState.checked(orderWatchPath)
        .flatMap(_.onOrderDeleted(name, orderId))
        .flatMap(o => updateOrderWatchState(
          o))
    }

    def nextEvents(toOrderAdded: ToOrderAdded, isDeletionMarkable: OrderId => Boolean)
    : View[KeyedEvent[OrderEvent.OrderCoreEvent]] =
      pathToOrderWatchState.values.view
        .flatMap(_.nextEvents(toOrderAdded, isDeletionMarkable))

    def applySnapshot(snapshot: OrderWatchState.ExternalOrderSnapshot): Checked[Self] =
      pathToOrderWatchState.checked(snapshot.orderWatchPath)
        .flatMap(_
          .applySnapshot(snapshot)
          .flatMap(watchState => updateOrderWatchState(
            watchState)))

    def finishRecovery: Checked[Self] =
      pathToOrderWatchState.values
        .toVector
        .traverse(_.finishRecovery)
        .flatMap(updateOrderWatchStates(_))
  }
}

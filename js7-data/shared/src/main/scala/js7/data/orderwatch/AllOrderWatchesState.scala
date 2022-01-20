package js7.data.orderwatch

import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.OrderAddedX
import js7.data.order.{OrderEvent, OrderId}
import js7.data.orderwatch.OrderWatchState.ToOrderAdded
import monix.reactive.Observable
import scala.collection.View

final case class AllOrderWatchesState(pathToOrderWatchState: Map[OrderWatchPath, OrderWatchState])
{
  def addOrderWatch(orderWatch: OrderWatch): Checked[AllOrderWatchesState] =
    pathToOrderWatchState.insert(orderWatch.key -> OrderWatchState(orderWatch))
      .map(o => copy(pathToOrderWatchState = o))

  def removeOrderWatch(orderWatchPath: OrderWatchPath): AllOrderWatchesState =
    copy(pathToOrderWatchState = pathToOrderWatchState - orderWatchPath)

  def changeOrderWatch(changed: OrderWatch): Checked[AllOrderWatchesState] =
    pathToOrderWatchState
      .checked(changed.key)
      .map(watchState =>
        copy(
          pathToOrderWatchState = pathToOrderWatchState + (changed.key -> watchState.copy(
            orderWatch = changed))))

  def onOrderWatchEvent(keyedEvent: KeyedEvent[OrderWatchEvent])
  : Checked[AllOrderWatchesState] =
    pathToOrderWatchState
      .checked(keyedEvent.key)
      .flatMap(_.applyOrderWatchEvent(keyedEvent.event))
      .map(updated => copy(
        pathToOrderWatchState = pathToOrderWatchState + (updated.id -> updated)))

  def onOrderAdded(keyedEvent: KeyedEvent[OrderAddedX]): Checked[AllOrderWatchesState] =
    keyedEvent.event.externalOrderKey match {
      case None => Right(this)
      case Some(externalOrderKey) => onOrderAdded(externalOrderKey, keyedEvent.key)
    }

  def onOrderAdded(externalOrderKey: ExternalOrderKey, orderId: OrderId): Checked[AllOrderWatchesState] = {
    import externalOrderKey.orderWatchPath
    pathToOrderWatchState
      .checked(orderWatchPath)
      .flatMap(_.onOrderAdded(externalOrderKey.name, orderId))
      .map(watchState => copy(
        pathToOrderWatchState = pathToOrderWatchState + (orderWatchPath -> watchState)))
  }

  def onOrderDeleted(externalOrderKey: ExternalOrderKey, orderId: OrderId)
  : Checked[AllOrderWatchesState] = {
    import externalOrderKey.{name, orderWatchPath}
    pathToOrderWatchState.checked(orderWatchPath)
      .flatMap(_.onOrderDeleted(name, orderId))
      .map(o => copy(
        pathToOrderWatchState = pathToOrderWatchState + (orderWatchPath -> o)))
  }

  def nextEvents(toOrderAdded: ToOrderAdded, isDeletionMarkable: OrderId => Boolean)
  : View[KeyedEvent[OrderEvent.OrderCoreEvent]] =
    pathToOrderWatchState.values.view
      .flatMap(_.nextEvents(toOrderAdded, isDeletionMarkable))

  def estimatedSnapshotSize =
    pathToOrderWatchState.view.values.map(_.estimatedSnapshotSize).sum

  def toSnapshot: Observable[Any] =
    Observable.fromIterable(pathToOrderWatchState.values).flatMap(_.toSnapshot)

  def applySnapshot(snapshot: OrderWatchState.ExternalOrderSnapshot): Checked[AllOrderWatchesState] =
    pathToOrderWatchState.checked(snapshot.orderWatchPath)
      .flatMap(_
        .applySnapshot(snapshot)
        .map(watchState => copy(
          pathToOrderWatchState = pathToOrderWatchState + (watchState.id -> watchState))))

  def finishRecovery: Checked[AllOrderWatchesState] =
    pathToOrderWatchState.values
      .toVector
      .traverse(_.finishRecovery)
      .map(_.view.map(o => o.id -> o).toMap)
      .map(o => copy(
        pathToOrderWatchState = o))
}

object AllOrderWatchesState
{
  val empty = new AllOrderWatchesState(Map.empty)
}

package js7.data.orderwatch

import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.item.ItemAttachedState.{Detached, NotDetached}
import js7.data.item.{ItemAttachedState, VersionId}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.workflow.WorkflowPath
import monix.reactive.Observable
import scala.collection.View

final case class AllOrderWatchesState(pathToOrderWatchState: Map[OrderWatchPath, OrderWatchState])
{
  def addOrderWatch(orderWatch: OrderWatch): Checked[AllOrderWatchesState] =
    pathToOrderWatchState.insert(orderWatch.id -> OrderWatchState(orderWatch))
      .map(o => copy(pathToOrderWatchState = o))

  def removeOrderWatch(orderWatchPath: OrderWatchPath): AllOrderWatchesState =
    copy(pathToOrderWatchState = pathToOrderWatchState - orderWatchPath)

  def changeOrderWatch(changed: OrderWatch): Checked[AllOrderWatchesState] =
    pathToOrderWatchState
      .checked(changed.id)
      .map(watchState =>
        copy(
          pathToOrderWatchState = pathToOrderWatchState + (changed.id -> watchState.copy(
            orderWatch = changed))))

  def onOrderWatchEvent(keyedEvent: KeyedEvent[OrderWatchEvent])
  : Checked[AllOrderWatchesState] =
    pathToOrderWatchState
      .checked(keyedEvent.key)
      .flatMap(_.applyOrderWatchEvent(keyedEvent.event))
      .map(updated => copy(
        pathToOrderWatchState = pathToOrderWatchState + (updated.id -> updated)))

  def updateAttachedState(orderWatchPath: OrderWatchPath, agentPath: AgentPath, attachedState: ItemAttachedState)
  : Checked[AllOrderWatchesState] =
    pathToOrderWatchState
      .checked(orderWatchPath)
      .map { watchState =>
        val updated = watchState.copy(agentPathToAttachedState =
          attachedState match {
            case Detached =>
              watchState.agentPathToAttachedState - agentPath
            case a: NotDetached =>
              watchState.agentPathToAttachedState + (agentPath -> a)
          })
        copy(
          pathToOrderWatchState = pathToOrderWatchState + (orderWatchPath -> updated))
      }

  def markAsDeleted(id: OrderWatchPath): Checked[AllOrderWatchesState] =
    pathToOrderWatchState
      .checked(id)
      .map(o => copy(
        pathToOrderWatchState = pathToOrderWatchState + (o.id -> o.copy(
          delete = true))))

  def onOrderAdded(keyedEvent: KeyedEvent[OrderAdded]): Checked[AllOrderWatchesState] =
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

  def onOrderEvent(externalOrderKey: ExternalOrderKey, keyedEvent: KeyedEvent[OrderCoreEvent])
  : Checked[AllOrderWatchesState] = {
    import externalOrderKey.{name, orderWatchPath}
    pathToOrderWatchState.checked(orderWatchPath)
      .flatMap(_.applyOrderEvent(name, keyedEvent))
      .map(o => copy(
        pathToOrderWatchState = pathToOrderWatchState + (orderWatchPath -> o)))
  }

  def nextEvents(workflowPathToVersionId: WorkflowPath => Option[VersionId])
  : View[KeyedEvent[OrderEvent.OrderCoreEvent]] =
    pathToOrderWatchState.values.view
      .flatMap(_.nextEvents(workflowPathToVersionId))

  def estimatedSnapshotSize =
    pathToOrderWatchState.view.values.map(_.estimatedSnapshotSize).sum

  def toSnapshot: Observable[OrderWatchState.Snapshot] =
    Observable.fromIterable(pathToOrderWatchState.values).flatMap(_.toSnapshot)

  def applySnapshot(snapshot: OrderWatchState.Snapshot): Checked[AllOrderWatchesState] =
    snapshot match {
      case snapshot: OrderWatchState.HeaderSnapshot =>
        val watchState = OrderWatchState.fromSnapshot(snapshot)
        pathToOrderWatchState.insert(watchState.id -> watchState)
          .map(o => copy(
            pathToOrderWatchState = o))

      case snapshot: OrderWatchState.ExternalOrderSnapshot =>
        pathToOrderWatchState.checked(snapshot.orderWatchPath)
          .flatMap(_
            .applySnapshot(snapshot)
            .map(watchState => copy(
              pathToOrderWatchState = pathToOrderWatchState + (watchState.id -> watchState))))
  }

  // TODO How about a Builder class ?
  def onEndOfRecovery: Checked[AllOrderWatchesState] =
    pathToOrderWatchState.values
      .toVector
      .traverse(_.onEndOfRecovery)
      .map(_.view.map(o => o.id -> o).toMap)
      .map(o => copy(
        pathToOrderWatchState = o))
}

object AllOrderWatchesState
{
  val empty = new AllOrderWatchesState(Map.empty)
}

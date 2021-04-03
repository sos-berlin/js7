package js7.data.orderwatch

import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentId
import js7.data.event.KeyedEvent
import js7.data.item.ItemAttachedState.{Detached, NotDetached}
import js7.data.item.{ItemAttachedState, VersionId}
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.workflow.WorkflowPath
import monix.reactive.Observable
import scala.collection.View

final case class AllOrderWatchesState(idToOrderWatchState: Map[OrderWatchId, OrderWatchState])
{
  def addOrderWatch(orderWatch: OrderWatch): Checked[AllOrderWatchesState] =
    idToOrderWatchState.insert(orderWatch.id -> OrderWatchState(orderWatch))
      .map(o => copy(idToOrderWatchState = o))

  def removeOrderWatch(orderWatchId: OrderWatchId): AllOrderWatchesState =
    copy(idToOrderWatchState = idToOrderWatchState - orderWatchId)

  def changeOrderWatch(changed: OrderWatch): Checked[AllOrderWatchesState] =
    idToOrderWatchState
      .checked(changed.id)
      .map(watchState =>
        copy(
          idToOrderWatchState = idToOrderWatchState + (changed.id -> watchState.copy(
            orderWatch = changed))))

  def onOrderWatchEvent(keyedEvent: KeyedEvent[OrderWatchEvent])
  : Checked[AllOrderWatchesState] =
    idToOrderWatchState
      .checked(keyedEvent.key)
      .flatMap(_.applyOrderWatchEvent(keyedEvent.event))
      .map(updated => copy(
        idToOrderWatchState = idToOrderWatchState + (updated.id -> updated)))

  def updateAttachedState(orderWatchId: OrderWatchId, agentId: AgentId, attachedState: ItemAttachedState)
  : Checked[AllOrderWatchesState] =
    idToOrderWatchState
      .checked(orderWatchId)
      .map { watchState =>
        val updated = watchState.copy(agentIdToAttachedState =
          attachedState match {
            case Detached =>
              watchState.agentIdToAttachedState - agentId
            case a: NotDetached =>
              watchState.agentIdToAttachedState + (agentId -> a)
          })
        copy(
          idToOrderWatchState = idToOrderWatchState + (orderWatchId -> updated))
      }

  def markAsDeleted(id: OrderWatchId): Checked[AllOrderWatchesState] =
    idToOrderWatchState
      .checked(id)
      .map(o => copy(
        idToOrderWatchState = idToOrderWatchState + (o.id -> o.copy(
          delete = true))))

  def onOrderAdded(keyedEvent: KeyedEvent[OrderAdded]): Checked[AllOrderWatchesState] =
    keyedEvent.event.externalOrderKey match {
      case None => Right(this)
      case Some(externalOrderKey) => onOrderAdded(externalOrderKey, keyedEvent.key)
    }

  def onOrderAdded(externalOrderKey: ExternalOrderKey, orderId: OrderId): Checked[AllOrderWatchesState] = {
    import externalOrderKey.orderWatchId
    idToOrderWatchState
      .checked(orderWatchId)
      .flatMap(_.onOrderAdded(externalOrderKey.name, orderId))
      .map(watchState => copy(
        idToOrderWatchState = idToOrderWatchState + (orderWatchId -> watchState)))
  }

  def onOrderEvent(externalOrderKey: ExternalOrderKey, keyedEvent: KeyedEvent[OrderCoreEvent])
  : Checked[AllOrderWatchesState] = {
    import externalOrderKey.{name, orderWatchId}
    idToOrderWatchState.checked(orderWatchId)
      .flatMap(_.applyOrderEvent(name, keyedEvent))
      .map(o => copy(
        idToOrderWatchState = idToOrderWatchState + (orderWatchId -> o)))
  }

  def nextEvents(workflowPathToVersionId: WorkflowPath => Option[VersionId])
  : View[KeyedEvent[OrderEvent.OrderCoreEvent]] =
    idToOrderWatchState.values.view
      .flatMap(_.nextEvents(workflowPathToVersionId))

  def estimatedSnapshotSize =
    idToOrderWatchState.view.values.map(_.estimatedSnapshotSize).sum

  def toSnapshot: Observable[OrderWatchState.Snapshot] =
    Observable.fromIterable(idToOrderWatchState.values).flatMap(_.toSnapshot)

  def applySnapshot(snapshot: OrderWatchState.Snapshot): Checked[AllOrderWatchesState] =
    snapshot match {
      case snapshot: OrderWatchState.HeaderSnapshot =>
        val watchState = OrderWatchState.fromSnapshot(snapshot)
        idToOrderWatchState.insert(watchState.id -> watchState)
          .map(o => copy(
            idToOrderWatchState = o))

      case snapshot: OrderWatchState.ExternalOrderSnapshot =>
        idToOrderWatchState.checked(snapshot.orderWatchId)
          .flatMap(_
            .applySnapshot(snapshot)
            .map(watchState => copy(
              idToOrderWatchState = idToOrderWatchState + (watchState.id -> watchState))))
  }

  // TODO How about a Builder class ?
  def onEndOfRecovery: Checked[AllOrderWatchesState] =
    idToOrderWatchState.values
      .toVector
      .traverse(_.onEndOfRecovery)
      .map(_.view.map(o => o.id -> o).toMap)
      .map(o => copy(
        idToOrderWatchState = o))
}

object AllOrderWatchesState
{
  val empty = new AllOrderWatchesState(Map.empty)
}

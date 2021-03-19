package js7.data.orderwatch

import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.{AgentId, AttachedState}
import js7.data.event.KeyedEvent
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderAdded, OrderCoreEvent}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.workflow.WorkflowPath
import monix.reactive.Observable

final case class AllOrderWatchesState(idToOrderWatchState: Map[OrderWatchId, OrderWatchState])
{
  def addOrderWatch(orderWatch: OrderWatch): Checked[AllOrderWatchesState] =
    idToOrderWatchState.insert(orderWatch.id -> OrderWatchState(orderWatch))
      .map(o => copy(idToOrderWatchState = o))

  def removeOrderWatch(orderWatchId: OrderWatchId): AllOrderWatchesState =
    copy(idToOrderWatchState = idToOrderWatchState - orderWatchId)

  def changeOrderWatch(orderWatch: OrderWatch): Checked[AllOrderWatchesState] =
    for (watchState <- idToOrderWatchState.checked(orderWatch.id))
      yield copy(
        idToOrderWatchState = idToOrderWatchState + (orderWatch.id -> watchState.copy(
          orderWatch = orderWatch)))

  def onOrderWatchEvent(keyedEvent: KeyedEvent[OrderWatchEvent])
  : Checked[AllOrderWatchesState] =
    idToOrderWatchState
      .checked(keyedEvent.key)
      .flatMap(_.applyOrderWatchEvent(keyedEvent.event))
      .map(updated => copy(
        idToOrderWatchState = idToOrderWatchState + (updated.id -> updated)))

  def updatedAttachedState(
    orderWatchId: OrderWatchId,
    agentId: AgentId,
    attachedState: Option[AttachedState])
  : Checked[AllOrderWatchesState] =
    idToOrderWatchState
      .checked(orderWatchId)
      .flatMap(watchState =>
        if (watchState.orderWatch.agentId != agentId)
          Left(Problem(
            s"updatedAttachedState $orderWatchId: agentId=${watchState.orderWatch.agentId} != agentId"))
        else Right(copy(
          idToOrderWatchState = idToOrderWatchState +
            (orderWatchId ->
              watchState.copy(
                attached = attachedState)))))

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
  : Seq[KeyedEvent[OrderEvent.OrderCoreEvent]] =
    idToOrderWatchState.values.view
      .flatMap(_.nextEvents(workflowPathToVersionId))
      .toVector

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

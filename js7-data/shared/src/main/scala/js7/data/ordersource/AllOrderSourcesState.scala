package js7.data.ordersource

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

final case class AllOrderSourcesState(idToOrderSourceState: Map[OrderSourceId, OrderSourceState])
{
  def addOrderSource(orderSource: OrderSource): Checked[AllOrderSourcesState] =
    idToOrderSourceState.insert(orderSource.id -> OrderSourceState(orderSource))
      .map(o => copy(idToOrderSourceState = o))

  def removeOrderSource(orderSourceId: OrderSourceId): AllOrderSourcesState =
    copy(idToOrderSourceState = idToOrderSourceState - orderSourceId)

  def changeOrderSource(orderSource: OrderSource): Checked[AllOrderSourcesState] =
    for (orderSourceState <- idToOrderSourceState.checked(orderSource.id))
      yield copy(
        idToOrderSourceState = idToOrderSourceState + (orderSource.id -> orderSourceState.copy(
          orderSource = orderSource)))

  def onOrderSourceEvent(keyedEvent: KeyedEvent[OrderSourceEvent])
  : Checked[AllOrderSourcesState] = {
    val id = keyedEvent.key
    idToOrderSourceState.checked(id)
      .flatMap(_.applyOrderSourceEvent(keyedEvent.event))
      .map(updated => copy(
        idToOrderSourceState = idToOrderSourceState + (id -> updated)))
  }

  def updatedAttachedState(
    orderSourceId: OrderSourceId,
    agentId: AgentId,
    attachedState: Option[AttachedState])
  : Checked[AllOrderSourcesState] =
    idToOrderSourceState
      .checked(orderSourceId)
      .flatMap(orderSourceState =>
        if (orderSourceState.orderSource.agentId != agentId)
          Left(Problem(
            s"updatedAttachedState $orderSourceId: agentId=${orderSourceState.orderSource.agentId} != agentId"))
        else Right(copy(
          idToOrderSourceState = idToOrderSourceState +
            (orderSourceId ->
              orderSourceState.copy(
                attached = attachedState)))))

  def onOrderAdded(keyedEvent: KeyedEvent[OrderAdded]): Checked[AllOrderSourcesState] =
    keyedEvent.event.sourceOrderKey match {
      case None => Right(this)
      case Some(sourceOrderKey) => onOrderAdded(sourceOrderKey, keyedEvent.key)
    }

  def onOrderAdded(sourceOrderKey: SourceOrderKey, orderId: OrderId): Checked[AllOrderSourcesState] = {
    import sourceOrderKey.orderSourceId
    idToOrderSourceState
      .checked(orderSourceId)
      .flatMap(_.onOrderAdded(sourceOrderKey.name, orderId))
      .map(orderSourceState => copy(
        idToOrderSourceState = idToOrderSourceState + (orderSourceId -> orderSourceState)))
  }

  def onOrderEvent(sourceOrderKey: SourceOrderKey, keyedEvent: KeyedEvent[OrderCoreEvent])
  : Checked[AllOrderSourcesState] = {
    import sourceOrderKey.{name, orderSourceId}
    idToOrderSourceState.checked(orderSourceId)
      .flatMap(_.applyOrderEvent(name, keyedEvent))
      .map(o => copy(
        idToOrderSourceState = idToOrderSourceState + (orderSourceId -> o)))
  }

  def nextEvents(workflowPathToVersionId: WorkflowPath => Option[VersionId])
  : Seq[KeyedEvent[OrderEvent.OrderCoreEvent]] =
    idToOrderSourceState.values.view
      .flatMap(_.nextEvents(workflowPathToVersionId))
      .toVector

  def estimatedSnapshotSize =
    idToOrderSourceState.view.values.map(_.estimatedSnapshotSize).sum

  def toSnapshot: Observable[OrderSourceState.Snapshot] =
    Observable.fromIterable(idToOrderSourceState.values).flatMap(_.toSnapshot)

  def applySnapshot(snapshot: OrderSourceState.Snapshot): Checked[AllOrderSourcesState] =
    snapshot match {
      case snapshot: OrderSourceState.HeaderSnapshot =>
        val orderSourceState = OrderSourceState.fromSnapshot(snapshot)
        idToOrderSourceState.insert(orderSourceState.id -> orderSourceState)
          .map(o => copy(
            idToOrderSourceState = o))

      case snapshot: OrderSourceState.SourceOrderSnapshot =>
        idToOrderSourceState.checked(snapshot.orderSourceId)
          .flatMap(orderSourceState => orderSourceState
            .applySnapshot(snapshot)
            .map(orderSourceState => copy(
              idToOrderSourceState = idToOrderSourceState + (orderSourceState.id -> orderSourceState))))
  }

  // TODO How about a Builder class ?
  def onEndOfRecovery: Checked[AllOrderSourcesState] =
    idToOrderSourceState.values
      .toVector
      .traverse(_.onEndOfRecovery)
      .map(_.view.map(o => o.id -> o).toMap)
      .map(o => copy(
        idToOrderSourceState = o))
}

object AllOrderSourcesState
{
  val empty = new AllOrderSourcesState(Map.empty)
}

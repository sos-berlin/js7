package js7.data.ordersource

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.circeutils.{CirceCodec, CirceObjectCodec}
import js7.base.problem.Checked
import js7.base.utils.Collections.RichMap
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AttachedState
import js7.data.item.SimpleItemState
import js7.data.order.OrderId
import js7.data.ordersource.OrderSourceState._
import monix.reactive.Observable

final case class OrderSourceState(
  orderSource: OrderSource,
  attached: Option[AttachedState] = None,
  sourceToOrderId: Map[SourceOrderName, OrderId] = Map.empty)
extends SimpleItemState
{
  def item = orderSource

  def id = orderSource.id

  type Revision = Int

  def assertUniqueness(): Unit =
    sourceToOrderId.view.values.checkUniqueness.orThrow

  def addOrderId(sourceOrderName: SourceOrderName, orderId: OrderId): Checked[OrderSourceState] =
    sourceToOrderId.insert(sourceOrderName -> orderId)
      .map(updated => copy(
        sourceToOrderId = updated))

  def removeOrderId(sourceOrderName: SourceOrderName, orderId: OrderId): OrderSourceState =
    copy(sourceToOrderId =
      sourceToOrderId - sourceOrderName)

  def estimatedSnapshotSize = 1

  def toSnapshot: Observable[Snapshot] =
    Observable.pure(HeaderSnapshot(orderSource, attached))
    // sourceToOrderId is redundant with Order.sourceOrderKey
}

object OrderSourceState
{
  def fromSnapshot(snapshot: HeaderSnapshot) =
    OrderSourceState(snapshot.orderSource, snapshot.attached)

  implicit def jsonCodec(implicit os: CirceCodec[OrderSource]): CirceObjectCodec[OrderSourceState] =
    deriveCodec[OrderSourceState]

  sealed trait Snapshot
  final case class HeaderSnapshot(
    orderSource: OrderSource,
    attached: Option[AttachedState] = None)
  extends Snapshot

  object Snapshot {
    implicit val jsonCodec = TypedJsonCodec[Snapshot](
      Subtype.named(deriveCodec[HeaderSnapshot], "OrderSourceState.Snapshot")
    )
  }
}

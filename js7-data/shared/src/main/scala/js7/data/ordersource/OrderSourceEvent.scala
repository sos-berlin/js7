package js7.data.ordersource

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.Event
import js7.data.value.NamedValues

sealed trait OrderSourceEvent extends Event
{
  type Key = OrderSourceId

  def sourceOrderName: SourceOrderName
}

object OrderSourceEvent
{
  // TODO OrderSource --> OrderWatch ?
  // TODO FileOrderSource --> FileOrderWatch ?
  // WatchedOrderEvent, WatchedOrderArised/Appeared, WatchedOrderVanished
  /** External Order arised, Controller is expected to add it. */
  final case class OrderSourceOrderArised(
    sourceOrderName: SourceOrderName,
    arguments: NamedValues = Map.empty)
  extends OrderSourceEvent

  /** External Order vanished, Controller should remove the added order. */
  final case class OrderSourceOrderVanished(sourceOrderName: SourceOrderName)
  extends OrderSourceEvent

  implicit val jsonCodec = TypedJsonCodec[OrderSourceEvent](
    Subtype(deriveCodec[OrderSourceOrderArised]),
    Subtype(deriveCodec[OrderSourceOrderVanished]))
}

package js7.data.ordersource

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.Event
import js7.data.value.NamedValues

sealed trait OrderSourceEvent extends Event
{
  type Key = OrderSourceId
}

object OrderSourceEvent
{
  sealed trait OrderSourceAgentEvent extends OrderSourceEvent
  sealed trait OrderSourceControllerEvent extends OrderSourceEvent

  // TODO OrderSource -> OrderWatch ?
  /** External Order arised, Controller is expected to add it. */
  final case class OrderSourceOrderArised(
    sourceOrderName: SourceOrderName,
    arguments: NamedValues = Map.empty)
  extends OrderSourceAgentEvent

  /** External Order vanished, Controller should remove the added order. */
  final case class OrderSourceOrderVanished(sourceOrderName: SourceOrderName)
  extends OrderSourceAgentEvent

  implicit val jsonCodec = TypedJsonCodec[OrderSourceEvent](
    Subtype(deriveCodec[OrderSourceOrderArised]),
    Subtype(deriveCodec[OrderSourceOrderVanished]))
}

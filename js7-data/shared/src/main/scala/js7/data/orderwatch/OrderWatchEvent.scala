package js7.data.orderwatch

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.Event
import js7.data.order.OrderId
import js7.data.value.NamedValues

sealed trait OrderWatchEvent extends Event.IsKeyBase[OrderWatchEvent]:
  val keyCompanion: OrderWatchEvent.type  = OrderWatchEvent

  def externalOrderName: ExternalOrderName


object OrderWatchEvent extends Event.CompanionForKey[OrderWatchPath, OrderWatchEvent]:
  implicit def implicitSelf: OrderWatchEvent.type = this

  // TODO Rename as ExternalOrderArose
  /** An external Order arose, Controller is expected to add an Order. */
  final case class ExternalOrderArised(
    externalOrderName: ExternalOrderName,
    orderId: OrderId,
    arguments: NamedValues = Map.empty)
  extends OrderWatchEvent

  /** External Order vanished, Controller should remove the added order. */
  final case class ExternalOrderVanished(externalOrderName: ExternalOrderName)
  extends OrderWatchEvent

  implicit val jsonCodec: TypedJsonCodec[OrderWatchEvent] = TypedJsonCodec(
    Subtype(deriveCodec[ExternalOrderArised]),
    Subtype(deriveCodec[ExternalOrderVanished]))

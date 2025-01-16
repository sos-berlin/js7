package js7.data.orderwatch

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.data.event.Event
import js7.data.order.OrderId
import js7.data.value.NamedValues

sealed trait OrderWatchEvent extends Event.IsKeyBase[OrderWatchEvent]:
  val keyCompanion: OrderWatchEvent.type  = OrderWatchEvent

  def externalOrderName: ExternalOrderName


object OrderWatchEvent extends Event.CompanionForKey[OrderWatchPath, OrderWatchEvent]:
  implicit def implicitSelf: OrderWatchEvent.type = this

  // TODO Rename as ExternalOrderArose or better ExternalOrderAppeared
  /** An external Order arose, Controller is expected to add an Order. */
  final case class ExternalOrderArised(
    externalOrderName: ExternalOrderName,
    orderId: OrderId,
    arguments: NamedValues = Map.empty)
  extends OrderWatchEvent:
    override def toString =
      s"ExternalOrderArised(${externalOrderName.string}, $orderId, $arguments)"

  /** The external order could not be added as an Order. */
  final case class ExternalOrderRejected(
    externalOrderName: ExternalOrderName,
    orderId: OrderId,
    problem: Problem)
  extends OrderWatchEvent:
    override def toString =
      s"ExternalOrderRejected(${externalOrderName.string}, $orderId, ⛔️$problem)"

  /** External Order vanished, Controller should remove the added order. */
  final case class ExternalOrderVanished(externalOrderName: ExternalOrderName)
  extends OrderWatchEvent:
    override def toString =
      s"ExternalOrderVanished(${externalOrderName.string})"


  implicit val jsonCodec: TypedJsonCodec[OrderWatchEvent] = TypedJsonCodec(
    Subtype(deriveCodec[ExternalOrderArised]),
    Subtype(deriveCodec[ExternalOrderRejected]),
    Subtype(deriveCodec[ExternalOrderVanished]))

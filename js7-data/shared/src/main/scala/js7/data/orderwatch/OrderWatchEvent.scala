package js7.data.orderwatch

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.Event
import js7.data.order.OrderId
import js7.data.value.NamedValues

sealed trait OrderWatchEvent extends Event.IsKeyBase[OrderWatchEvent]:
  val keyCompanion: OrderWatchEvent.type  = OrderWatchEvent

  def externalOrderName: ExternalOrderName


object OrderWatchEvent extends Event.CompanionForKey[OrderWatchPath, OrderWatchEvent]:
  implicit def implicitSelf: OrderWatchEvent.type = this

  /** An external Order appeared, Controller is expected to add an Order. */
  final case class ExternalOrderAppeared(
    externalOrderName: ExternalOrderName,
    arguments: NamedValues = Map.empty,
    orderId: Option[OrderId] = None/*COMPATIBLE with v2.7.3*/)
    extends OrderWatchEvent:
    override def toString =
      s"ExternalOrderAppeared(${externalOrderName.string}, ${
        arguments.map((k, v) => s"$k=$v").mkStringLimited(3)
      })"

  /** The external order could not be added as an Order. */
  final case class ExternalOrderRejected(externalOrderName: ExternalOrderName, problem: Problem)
  extends OrderWatchEvent:
    override def toString =
      s"ExternalOrderRejected(${externalOrderName.string}, ⛔️ $problem)"

  /** External Order vanished, Controller should remove the added order. */
  final case class ExternalOrderVanished(externalOrderName: ExternalOrderName)
  extends OrderWatchEvent:
    override def toString =
      s"ExternalOrderVanished(${externalOrderName.string})"


  implicit val jsonCodec: TypedJsonCodec[OrderWatchEvent] = TypedJsonCodec(
    Subtype(deriveCodec[ExternalOrderAppeared], aliases = Seq(
      "ExternalOrderArised"/*COMPATIBLE with v2.7.3*/)),
    Subtype(deriveCodec[ExternalOrderRejected]),
    Subtype(deriveCodec[ExternalOrderVanished]))

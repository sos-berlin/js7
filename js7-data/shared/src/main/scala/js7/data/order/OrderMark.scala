package js7.data.order

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Big
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.order.OrderEvent.OrderResumed
import js7.data.workflow.position.Position

sealed trait OrderMark

object OrderMark
{
  final case class Cancelling(mode: CancellationMode)
  extends OrderMark

  final case class Suspending(mode: SuspensionMode = SuspensionMode.standard)
  extends OrderMark

  final case class Resuming(
    position: Option[Position] = None,
    historicOperations: Seq[OrderResumed.HistoryOperation] = Nil,
    asSucceeded: Boolean = false)
  extends OrderMark with Big

  implicit val jsonCodec: TypedJsonCodec[OrderMark] = TypedJsonCodec(
    Subtype(deriveCodec[Cancelling]),
    Subtype(deriveCodec[Suspending]),
    Subtype(deriveConfiguredCodec[Resuming]))
}

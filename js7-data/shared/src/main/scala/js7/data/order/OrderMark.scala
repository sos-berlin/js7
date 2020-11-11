package js7.data.order

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.command.{CancelMode, SuspendMode}
import js7.data.workflow.position.Position

sealed trait OrderMark

object OrderMark
{
  final case class Cancelling(mode: CancelMode)
  extends OrderMark

  final case class Suspending(mode: SuspendMode = SuspendMode.default)
  extends OrderMark

  case class Resuming(position: Option[Position] = None) extends OrderMark

  implicit val jsonCodec = TypedJsonCodec.apply[OrderMark](
    Subtype(deriveCodec[Cancelling]),
    Subtype(deriveCodec[Suspending]),
    Subtype(deriveCodec[Resuming]))
}

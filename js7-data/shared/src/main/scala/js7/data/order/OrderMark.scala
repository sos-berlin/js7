package js7.data.order

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.command.CancelMode

sealed trait OrderMark

object OrderMark
{
  final case class Cancelling(mode: CancelMode)
  extends OrderMark

  type Suspending = Suspending.type
  case object Suspending extends OrderMark

  type Resuming = Resuming.type
  case object Resuming extends OrderMark

  implicit val jsonCodec = TypedJsonCodec.apply[OrderMark](
    Subtype(deriveCodec[Cancelling]),
    Subtype(Suspending),
    Subtype(Resuming))
}

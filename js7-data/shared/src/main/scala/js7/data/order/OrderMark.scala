package js7.data.order

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.workflow.position.Position

sealed trait OrderMark

object OrderMark
{
  final case class Cancelling(mode: CancellationMode)
  extends OrderMark

  final case class Suspending(mode: SuspensionMode = SuspensionMode.standard)
  extends OrderMark

  case class Resuming(
    position: Option[Position] = None,
    historicOutcomes: Option[Seq[HistoricOutcome]] = None)
  extends OrderMark

  implicit val jsonCodec = TypedJsonCodec[OrderMark](
    Subtype(deriveCodec[Cancelling]),
    Subtype(deriveCodec[Suspending]),
    Subtype(deriveCodec[Resuming]))
}

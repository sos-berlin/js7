package js7.data.order

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Big
import js7.base.utils.ScalaUtils.functionCallToString
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.order.OrderEvent.OrderResumed
import js7.data.workflow.position.Position

sealed trait OrderMark


object OrderMark:

  final case class Cancelling(mode: CancellationMode)
  extends OrderMark:
    override def toString =
      functionCallToString("Cancelling", (mode != CancellationMode.Default) ? mode)

  final case class Suspending(mode: SuspensionMode = SuspensionMode.standard)
  extends OrderMark:
    override def toString =
      functionCallToString("Suspending", (mode != SuspensionMode.standard) ? mode)

  final case class Resuming(
    position: Option[Position] = None,
    // TODO rename as historyOperations like in OrderResumed
    historicOperations: Seq[OrderResumed.HistoryOperation] = Nil,
    asSucceeded: Boolean = false,
    restartKilledJob: Boolean = false)
  extends OrderMark, Big:
    override def toString =
      functionCallToString("Resuming",
        position.view, historicOperations,
        asSucceeded ? "asSucceeded",
        restartKilledJob ? "restartKilledJob")

  /** Order shall no longer wait but resume, if at the requested Position.
   * @param position is the dynamic Order Position (with arguments).
   */
  final case class Go(position: Position)
  extends OrderMark

  implicit val jsonCodec: TypedJsonCodec[OrderMark] = TypedJsonCodec(
    Subtype(deriveCodec[Cancelling]),
    Subtype(deriveCodec[Suspending]),
    Subtype(deriveCodecWithDefaults[Resuming]),
    Subtype(deriveCodec[Go]))

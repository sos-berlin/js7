package js7.data.order

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Big
import js7.base.utils.ScalaUtils.parameterListToString
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.order.OrderEvent.OrderResumed
import js7.data.workflow.position.Position

sealed trait OrderMark


object OrderMark:

  final case class Cancelling(mode: CancellationMode)
  extends OrderMark:
    override def toString = "Cancelling" +
      parameterListToString((mode != CancellationMode.Default) ? mode)

  final case class Suspending(mode: SuspensionMode = SuspensionMode.standard)
  extends OrderMark:
    override def toString = "Suspending" +
      parameterListToString((mode != SuspensionMode.standard) ? mode)

  final case class Resuming(
    position: Option[Position] = None,
    // TODO rename as historyOperations like in OrderResumed
    historicOperations: Seq[OrderResumed.HistoryOperation] = Nil,
    asSucceeded: Boolean = false)
  extends OrderMark, Big:
    override def toString = "Resuming" +
      parameterListToString(position.view, historicOperations, asSucceeded ? "asSucceeded")

  /** Order shall no longer wait but resume, if at the requested Position.
   * @param position is the dynamic Order Position (with arguments).
   */
  final case class Go(position: Position)
  extends OrderMark

  implicit val jsonCodec: TypedJsonCodec[OrderMark] = TypedJsonCodec(
    Subtype(deriveCodec[Cancelling]),
    Subtype(deriveCodec[Suspending]),
    Subtype(deriveConfiguredCodec[Resuming]),
    Subtype(deriveCodec[Go]))

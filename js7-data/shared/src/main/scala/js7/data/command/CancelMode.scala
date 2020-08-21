package js7.data.command

import io.circe.generic.extras.Configuration.default.withDefaults
import js7.base.circeutils.CirceUtils.{deriveCodec, deriveConfiguredCodec}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.process.ProcessSignal
import js7.base.process.ProcessSignal.SIGTERM
import js7.data.workflow.position.WorkflowPosition

/**
  * @author Joacim Zschimmer
  */
sealed trait CancelMode

object CancelMode
{
  val Default = NotStarted

  case object NotStarted extends CancelMode

  final case class FreshOrStarted(kill: Option[Kill] = None) extends CancelMode

  final case class Kill(signal: ProcessSignal = SIGTERM, workflowPosition: Option[WorkflowPosition] = None)
  object Kill {
    private implicit val customConfig = withDefaults
    implicit val jsonCodec = deriveConfiguredCodec[Kill]
  }

  implicit val jsonCodec = TypedJsonCodec[CancelMode](
    Subtype(NotStarted),
    Subtype(deriveCodec[FreshOrStarted]))
}

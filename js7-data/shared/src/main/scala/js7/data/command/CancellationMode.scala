package js7.data.command

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.workflow.position.WorkflowPosition

/**
  * @author Joacim Zschimmer
  */
sealed trait CancellationMode

object CancellationMode
{
  val Default = FreshOrStarted(None)

  case object FreshOnly extends CancellationMode

  final case class FreshOrStarted(kill: Option[Kill] = None) extends CancellationMode

  def kill(immediately: Boolean = false, workflowPosition: Option[WorkflowPosition] = None): FreshOrStarted =
    FreshOrStarted(Some(Kill(immediately = immediately, workflowPosition = workflowPosition)))

  final case class Kill(immediately: Boolean = false, workflowPosition: Option[WorkflowPosition] = None)
  object Kill {
    private implicit val customConfig: Configuration = Configuration.default.withDefaults
    implicit val jsonCodec = deriveConfiguredCodec[Kill]
  }

  implicit val jsonCodec: TypedJsonCodec[CancellationMode] = TypedJsonCodec(
    Subtype(FreshOnly),
    Subtype(deriveCodec[FreshOrStarted]))
}

package js7.data.workflow.position

import io.circe.generic.JsonCodec
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.workflow.WorkflowId

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class WorkflowPosition(workflowId: WorkflowId, position: Position)
{
  lazy val normalized = reuseIfEqual(this, workflowId /: position.normalized)

  override def toString = s"${workflowId.toSimpleString}:$position"
}

object WorkflowPosition
{
  val jsonEncoder: Encoder[WorkflowPosition] = deriveEncoder[WorkflowPosition]
  val jsonDecoder: Decoder[WorkflowPosition] = deriveDecoder[WorkflowPosition]
}

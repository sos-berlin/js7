package js7.data.workflow.position

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.workflow.WorkflowId

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowPosition(workflowId: WorkflowId, position: Position)
{
  lazy val normalized = reuseIfEqual(this, workflowId /: position.normalized)

  override def toString = s"${workflowId.toSimpleString}:$position"
}

object WorkflowPosition
{
  implicit val jsonCodec: Codec.AsObject[WorkflowPosition] = deriveCodec
}

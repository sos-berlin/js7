package js7.data.workflow.position

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.workflow.WorkflowId
import scala.language.implicitConversions

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
  // TODO Should be explicit
  implicit def apply(workflowId: WorkflowId): WorkflowPosition =
    WorkflowPosition(workflowId, Position.First)

  // TODO require workflowId.versionId != VersionId.Anonymous ?

  implicit val jsonCodec: Codec.AsObject[WorkflowPosition] = deriveCodec
}

package js7.data.workflow

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowOverview(
  path: WorkflowPath)


object WorkflowOverview:
  def fromWorkflow(workflow: Workflow) = WorkflowOverview(path = workflow.path)

  implicit val jsonCodec: Codec.AsObject[WorkflowOverview] = deriveCodec[WorkflowOverview]

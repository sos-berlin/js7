package js7.controller.workflow

import io.circe.Json
import js7.base.circeutils.CirceUtils._
import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.core.item.InventoryItemReader
import js7.data.item.SourceType
import js7.data.workflow.{Workflow, WorkflowId, WorkflowParser}

/**
  * @author Joacim Zschimmer
  */
object WorkflowReader extends InventoryItemReader
{
  val companion = Workflow

  def read(workflowId: WorkflowId, source: ByteArray) = {
    case t: SourceType.JsonLike =>
      readAnonymousJsonLike(t, source).map(_ withId workflowId)

    case SourceType.Txt =>
      WorkflowParser.parse(source.utf8String).map(_ withId workflowId)
  }

  override def convertFromJson(json: Json): Checked[Workflow] =
    Workflow.topJsonDecoder.decodeJson(json).toChecked
}

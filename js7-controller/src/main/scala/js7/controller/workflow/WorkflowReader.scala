package js7.controller.workflow

import io.circe.Json
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.core.item.VersionedItemReader
import js7.data.item.SourceType
import js7.data.workflow.{Workflow, WorkflowId, WorkflowParser}

/**
  * @author Joacim Zschimmer
  */
object WorkflowReader extends VersionedItemReader:
  val companion: Workflow.type = Workflow

  def read(workflowId: WorkflowId, source: ByteArray): PartialFunction[SourceType, Checked[Workflow]] =
    case t: SourceType.JsonLike =>
      readAnonymousJsonLike(t, source).map(_ withId workflowId)

    case SourceType.Txt =>
      WorkflowParser.parse(source.utf8String).map(_ withId workflowId)

  override def convertFromJson(json: Json): Checked[Workflow] =
    Workflow.topJsonDecoder.decodeJson(json).toChecked

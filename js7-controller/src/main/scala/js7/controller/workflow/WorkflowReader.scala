package js7.controller.workflow

import akka.util.ByteString
import io.circe.Json
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked
import js7.core.filebased.FileBasedReader
import js7.data.filebased.SourceType
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.{Workflow, WorkflowId}

/**
  * @author Joacim Zschimmer
  */
object WorkflowReader extends FileBasedReader
{
  val companion = Workflow

  def read(workflowId: WorkflowId, source: ByteString) = {
    case t: SourceType.JsonLike =>
      readAnonymousJsonLike(t, source).map(_ withId workflowId)

    case SourceType.Txt =>
      WorkflowParser.parse(source.utf8String) map (_ withId workflowId)
  }

  override def convertFromJson(json: Json): Checked[Workflow] =
    Workflow.topJsonDecoder.decodeJson(json).toChecked
}

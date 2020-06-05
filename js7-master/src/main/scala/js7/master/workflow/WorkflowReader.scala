package js7.master.workflow

import akka.util.ByteString
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked
import js7.core.filebased.FileBasedReader
import js7.data.filebased.SourceType
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.{Workflow, WorkflowId}
import io.circe.Json

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

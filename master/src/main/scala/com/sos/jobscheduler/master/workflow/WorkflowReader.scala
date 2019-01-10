package com.sos.jobscheduler.master.workflow

import akka.util.ByteString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId}
import io.circe.Json

/**
  * @author Joacim Zschimmer
  */
object WorkflowReader extends FileBasedReader
{
  val companion = Workflow

  def read(workflowId: WorkflowId, source: ByteString) = {
    case t: SourceType.JsonLike ⇒
      readAnonymousJsonLike(t, source).map(_ withId workflowId)

    case SourceType.Txt ⇒
      WorkflowParser.parse(source.utf8String) map (_ withId workflowId)
  }

  override def convertFromJson(json: Json): Checked[Workflow] =
    Workflow.topJsonDecoder.decodeJson(json).toSimpleChecked
}

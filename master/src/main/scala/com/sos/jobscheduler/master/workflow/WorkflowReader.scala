package com.sos.jobscheduler.master.workflow

import akka.util.ByteString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.simpleByteStringSource
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId}
import io.circe.parser.{parse ⇒ parseJson}

/**
  * @author Joacim Zschimmer
  */
object WorkflowReader extends FileBasedReader
{
  val companion = Workflow

  def read(workflowId: WorkflowId, source: ByteString) =
    Function.unlift { sourceType: SourceType ⇒
      readWorkflow(FolderPath.parentOf(workflowId.path), source).lift(sourceType) map (_ map (_.copy(id = workflowId)))
    }

  private def readWorkflow(folderPath: FolderPath, source: ByteString): PartialFunction[SourceType, Checked[Workflow]] = {
    case SourceType.Json ⇒ parseJson(source.utf8String).toSimpleChecked flatMap (_.as[Workflow].toSimpleChecked)
    case SourceType.Txt ⇒ WorkflowParser.parse(source.utf8String)
    case SourceType.Xml ⇒ LegacyJobchainXmlParser.parseXml(folderPath, simpleByteStringSource(source))
  }
}

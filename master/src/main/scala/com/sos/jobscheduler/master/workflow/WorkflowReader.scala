package com.sos.jobscheduler.master.workflow

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichCirceString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.simpleByteStringSource
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.core.workflow.notation.WorkflowParser
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.order.LegacyJobchainXmlParser

/**
  * @author Joacim Zschimmer
  */
object WorkflowReader extends FileBasedReader
{
  val fileBasedCompanion = Workflow.Named

  def read(workflowPath: WorkflowPath, source: ByteString) = {
    case sourceType if readWorkflow(workflowPath, source) isDefinedAt sourceType ⇒
      readWorkflow(workflowPath, source)(sourceType) map (Workflow.Named(workflowPath, _))
  }

  private def readWorkflow(workflowPath: WorkflowPath, source: ByteString): PartialFunction[SourceType, Checked[Workflow]] = {
    case SourceType.Json ⇒ source.utf8String.parseJson.as[Workflow].toChecked
    case SourceType.Txt ⇒ WorkflowParser.parse(source.utf8String)
    case SourceType.Xml ⇒ LegacyJobchainXmlParser.parseXml(FolderPath.parentOf(workflowPath), simpleByteStringSource(source))
  }
}

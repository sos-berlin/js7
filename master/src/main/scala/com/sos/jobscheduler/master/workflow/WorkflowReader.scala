package com.sos.jobscheduler.master.workflow

import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.simpleByteStringSource
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId}

/**
  * @author Joacim Zschimmer
  */
object WorkflowReader extends FileBasedReader
{
  val companion = Workflow

  def read(workflowId: WorkflowId, source: ByteString) = {
    case t: SourceType.JsonLike ⇒
      readAnonymousJsonLike[Workflow](t, source) map (_ withId workflowId)

    case SourceType.Txt ⇒
      WorkflowParser.parse(source.utf8String) map (_ withId workflowId)

    case SourceType.Xml ⇒
      val folderPath = FolderPath.parentOf(workflowId.path)
      LegacyJobchainXmlParser.parseXml(folderPath, simpleByteStringSource(source)) map (_ withId workflowId)
  }
}

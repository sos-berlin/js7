package com.sos.jobscheduler.master.workflow

import akka.util.ByteString
import com.sos.jobscheduler.base.problem.Checked
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

  def read(workflowId: WorkflowId, source: ByteString) =
    Function.unlift { sourceType: SourceType ⇒
      readWorkflow(FolderPath.parentOf(workflowId.path), source).lift(sourceType) map (_ map (_ withId workflowId))
    }

  private def readWorkflow(folderPath: FolderPath, source: ByteString): PartialFunction[SourceType, Checked[Workflow]] = {
    case SourceType.Json ⇒
      readAnonymousJson[Workflow](source)

    case SourceType.Txt ⇒
      WorkflowParser.parse(source.utf8String)

    case SourceType.Xml ⇒
      LegacyJobchainXmlParser.parseXml(folderPath, simpleByteStringSource(source))
  }
}

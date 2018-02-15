package com.sos.jobscheduler.master.agent

import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.simpleByteStringSource
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.master.order.agent.{Agent, AgentXmlParser}

/**
  * @author Joacim Zschimmer
  */
object AgentReader extends FileBasedReader
{
  val fileBasedCompanion = Agent

  def read(agentPath: AgentPath, source: ByteString) = {
    case SourceType.Xml ⇒ AgentXmlParser.parseXml(agentPath, simpleByteStringSource(source))
  }
}

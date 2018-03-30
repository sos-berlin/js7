package com.sos.jobscheduler.master.agent

import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.simpleByteStringSource
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.agent.{Agent, AgentId}
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.master.order.agent.AgentXmlParser

/**
  * @author Joacim Zschimmer
  */
object AgentReader extends FileBasedReader
{
  val companion = Agent

  def read(agentId: AgentId, source: ByteString) = {
    case SourceType.Xml ⇒ AgentXmlParser.parseXml(agentId, simpleByteStringSource(source))
  }
}

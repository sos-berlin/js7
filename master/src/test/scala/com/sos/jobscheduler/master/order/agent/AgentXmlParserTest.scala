package com.sos.jobscheduler.master.order.agent

import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.engine2.agent.AgentPath
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentXmlParserTest extends FreeSpec {

  "AgentXmlParser" in {
    val xml = <agent uri="http://localhost"/>
    val path = AgentPath("/FOLDER/AGENT")
    assert(AgentXmlParser.parseXml(path, xml) == Agent(path, uri = "http://localhost"))
  }
}

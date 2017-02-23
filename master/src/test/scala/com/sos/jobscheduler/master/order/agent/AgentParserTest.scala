package com.sos.jobscheduler.master.order.agent

import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.engine2.agent.AgentPath
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentParserTest extends FreeSpec {

  "AgentParser" in {
    val xml = <agent uri="http://localhost"/>
    val path = AgentPath("/FOLDER/AGENT")
    assert(AgentParser.parseXml(path, xml) == Agent(path, uri = "http://localhost"))
  }
}

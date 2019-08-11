package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentRefXmlParserTest extends FreeSpec {

  "AgentRefXmlParser" in {
    val xml = <agent uri="http://localhost"/>
    val id = AgentRefPath("/FOLDER/AGENT") ~ "VERSION"
    assert(AgentRefXmlParser.parseXml(id, xml) == Right(AgentRef(id, uri = "http://localhost")))
  }
}

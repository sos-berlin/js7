package com.sos.jobscheduler.master.order.agent

import cats.data.Validated.Valid
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.agent.AgentPath
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentXmlParserTest extends FreeSpec {

  "AgentXmlParser" in {
    val xml = <agent uri="http://localhost"/>
    val path = AgentPath("/FOLDER/AGENT")
    assert(AgentXmlParser.parseXml(path, xml) == Valid(Agent(path, uri = "http://localhost")))
  }
}

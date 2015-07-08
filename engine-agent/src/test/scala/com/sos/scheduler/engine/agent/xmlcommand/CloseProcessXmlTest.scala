package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands.CloseProcess
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class CloseProcessXmlTest extends FreeSpec {

  "Parse XML for CloseProcess" in {
    val commandElem = <remote_scheduler.remote_task.close process_id="111222333444555666" kill="true"/>
    val command = ScalaXMLEventReader.parseElem(commandElem)(CloseProcessXml.parseXml)
    assert(command == CloseProcess(AgentProcessId("111222333444555666"), kill = true))
  }
}

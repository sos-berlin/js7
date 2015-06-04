package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands.SendProcessSignal
import com.sos.scheduler.engine.base.process.ProcessSignal._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class SendProcessSignalXmlTest extends FreeSpec {

  "Parse XML for SendProcessSignal" in {
    val commandElem = <remote_scheduler.remote_task.kill process_id="111222333444555666" signal="SIGTERM"/>
    val command = ScalaXMLEventReader.parseElem(commandElem)(SendProcessSignalXml.parseXml)
    assert(command == SendProcessSignal(AgentProcessId(111222333444555666L), SIGTERM))
  }
}

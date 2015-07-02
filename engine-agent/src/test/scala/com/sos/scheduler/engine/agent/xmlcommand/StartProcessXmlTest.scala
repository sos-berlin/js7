package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.commands.{StartSeparateProcess, StartThread}
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class StartProcessXmlTest extends FreeSpec {

  "Parse XML as StartThread" in {
    val commandElem = <remote_scheduler.start_remote_task kind="process" ip_address="0.0.0.0" tcp_port="999"/>
    val command = ScalaXMLEventReader.parseElem(commandElem)(StartProcessXml.parseXml)
    assert(command == StartThread(controllerAddressOption = Some("0.0.0.0:999")))
  }

  "Parse XML as StartSeparateProcess" in {
    val commandElem = <remote_scheduler.start_remote_task ip_address="0.0.0.0" tcp_port="999" java_options="OPTIONS" java_classpath="CLASSPATH"/>
    val command = ScalaXMLEventReader.parseElem(commandElem)(StartProcessXml.parseXml)
    assert(command == StartSeparateProcess(controllerAddressOption = Some("0.0.0.0:999"), javaOptions = "OPTIONS", javaClasspath = "CLASSPATH"))
  }
}

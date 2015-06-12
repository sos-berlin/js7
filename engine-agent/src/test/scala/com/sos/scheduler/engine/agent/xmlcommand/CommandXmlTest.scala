package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.common.time.ScalaTime._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class CommandXmlTest extends FreeSpec {
  private val ipAddress = "127.0.0.1"

  "StartRemoteTask" in {
    intercept[Exception] { parse(<remote_scheduler.start_remote_task/>) }
    parse(<remote_scheduler.start_remote_task ip_address={ipAddress} tcp_port="999" java_options="OPTIONS" java_classpath="CLASSPATH"/>) shouldEqual
      StartSeparateProcess(controllerAddress = s"$ipAddress:999", javaOptions = "OPTIONS", javaClasspath = "CLASSPATH")
    parse(<remote_scheduler.start_remote_task ip_address={ipAddress} tcp_port="999" kind="process"/>) shouldEqual
      StartThread(controllerAddress = s"$ipAddress:999")
    intercept[Exception] { parse(<remote_scheduler.start_remote_task ip_address={ipAddress} tcp_port="-1"/>) }
    intercept[Exception] { parse(<remote_scheduler.start_remote_task ip_address={ipAddress} tcp_port="0"/>) }
    intercept[Exception] { parse(<remote_scheduler.start_remote_task ip_address={ipAddress} tcp_port="65536"/>) }
    parse(<remote_scheduler.start_remote_task ip_address={ipAddress}  tcp_port="65535"/>)
  }

  "CloseRemoteTask" in {
    intercept[Exception] { parse(<remote_scheduler.remote_task.close/>) }
    parse(<remote_scheduler.remote_task.close process_id="111222333444555666"/>) shouldEqual
      CloseProcess(AgentProcessId(111222333444555666L), kill = false)
    parse(<remote_scheduler.remote_task.close process_id="111222333444555666" kill="true"/>) shouldEqual
      CloseProcess(AgentProcessId(111222333444555666L), kill = true)
  }

  "Terminate" in {
    intercept[Exception] { parse(<agent.terminate/>) }
    parse(<agent.terminate cmd="terminate"/>) shouldEqual Terminate(sigtermProcesses = false)
    parse(<agent.terminate cmd="terminate" timeout="999"/>) shouldEqual Terminate(sigtermProcesses = false, sigkillProcessesAfter = 999.s)
  }

  "AbortImmediately" in {
    parse(<agent.terminate cmd="abort_immediately"/>) shouldEqual AbortImmediately
  }

  private def parse(elem: xml.Elem) = CommandXml.parseString(elem.toString())
}
